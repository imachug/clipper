/*
    Clipper: C++ CLI library
    Copyright (C) 2022  Alisa Sireneva

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef CLIPPER_HPP
#define CLIPPER_HPP

#include <charconv>
#include <cstddef>
#include <iostream>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace clipper {
using namespace std::literals::string_literals;

/// Denotes an incompatibility in CLI arguments passed by the user.
///
/// what() should end with a full stop.
class CLIArgumentParsingError : public std::runtime_error {
    using std::runtime_error::runtime_error;
};

template <typename Arg, size_t Start = 0, typename... FnTypes>
static auto MapFunctionComposition(std::tuple<FnTypes...> fns, Arg arg) {
    if constexpr (Start == sizeof...(FnTypes)) {
        return arg;
    } else {
        return std::get<Start>(fns)(MapFunctionComposition<Arg, Start + 1, FnTypes...>(fns, std::move(arg)));
    }
}

/// A functor for parsing std::string_view into integers, booleans, and other
/// types.
///
/// Users may specialize StringParser for their own types, e.g.:
///
/// \code{.cpp}
/// template<typename T, typename U>
/// struct StringParser<std::pair<T, U>> {
///     StringParser<T> t_parser;
///     StringParser<U> u_parser;
///     std::pair<T, U> operator()(std::string_view s) const {
///         size_t offset = s.find('=');
///         if(offset == std::string_view::npos) {
///             throw CLIArgumentParsingError("Missing '=' in an argument.");
///         }
///         return {
///             t_parser(s.substr(0, offset)),
///             u_parser(s.substr(offset + 1))
///         };
///     }
/// };
/// \endcode
///
/// ::CLIArgumentParsingError should be thrown in case of error so that an
/// informative error message can be shown.
template <typename T, typename Enable = void>
struct StringParser {
    T operator()(std::string_view s) const {
        static_assert(sizeof(T) < 0, "String parsing is not specialized for this type");
    }
};

// A trivial implementation for the sake of idempotence
template <>
struct StringParser<std::string_view> {
    std::string_view operator()(std::string_view s) const {
        return s;
    }
};

template <typename T>
struct StringParser<T, std::enable_if_t<std::is_arithmetic_v<T>>> {
    T operator()(std::string_view s) const {
        T value{};
        auto result = std::from_chars(s.begin(), s.end(), value, 10);
        if (result.ec != std::errc{} || result.ptr != s.end()) {
            throw CLIArgumentParsingError("\""s + s.data() + "\" is not a valid number.");
        }
        return value;
    }
};

template <>
struct StringParser<bool> {
    static const inline std::array YES_STRINGS{"1", "y", "yes", "true"};
    static const inline std::array NO_STRINGS{"0", "n", "no", "false"};

    bool operator()(std::string_view s) const {
        for (auto [value, strings] :
             std::array{std::make_pair(true, std::ref(YES_STRINGS)), std::make_pair(false, std::ref(NO_STRINGS))}) {
            if (std::any_of(strings.begin(), strings.end(), [&](std::string_view candidate) {
                    return std::equal(s.begin(), s.end(), candidate.begin(), candidate.end(),
                                      [](unsigned char a, unsigned char b) { return std::tolower(a) == b; });
                })) {
                return value;
            }
        }
        throw CLIArgumentParsingError("\""s + s.data() + "\" is not a valid boolean.");
    }
};

/// An extension of StringParser that parses std::vector and std::optional
/// recursively.
template <typename ElementType>
struct RecursiveStringParser {
    StringParser<ElementType> parser;

    ElementType operator()(std::string_view s) const {
        return parser(s);
    }

    template <typename T>
    auto operator()(const std::vector<T>& value) const {
        std::vector<decltype((*this)(value[0]))> result(value.size());
        std::transform(value.begin(), value.end(), result.begin(), std::ref(*this));
        return result;
    }

    template <typename T>
    auto operator()(const std::optional<T>& value) const -> std::optional<decltype((*this)(*value))> {
        if (value) {
            return (*this)(*value);
        } else {
            return std::nullopt;
        }
    }
};

/// Users may specialize StringParser for their own types, e.g.:
/// The kind of values and arguments an option expects, affecting static typing.
enum class CLIOptionKind {
    UNKNOWN,                ///< A default value when CLIOption is constructed, replaced by a
                            ///< real value shortly afterwards.
    FLAG,                   ///< The option takes no arguments, e.g. -v or --verbose.
    SINGLE_VALUE,           ///< The option takes exactly one argument, e.g. -o<path>, -o
                            ///< <path>, --output=<path>, or
                            ///< --output <path>.
    OPTIONAL_SINGLE_VALUE,  ///< The option takes either one or zero arguments,
                            ///< e.g. -h vs -h<topic> or -h <topic>, or
                            ///< --help vs --help=<topic> or --help <topic>.
    MULTIPLE_VALUES         ///< The option might take more than one argument, e.g.
                            ///< <file...>.
};

/// Configures a single CLI option with builder syntax.
///
/// The simplest way to configure an option is as follows:
///
/// \code{.cpp}
/// CLIOption{"verbose", "Enables verbose output"}.Short('v').Flag()
/// \encode
///
/// This declares an option named "verbose" (this name will be used in error
/// messages and help) with an appropriate description (used in help), enabled
/// by `-v`. Flag() declares that this option does not accept an argument and is
/// parsed as a boolean value (present/not present).
///
/// A slightly more complicated example for the same option is:
///
/// \code{.cpp}
/// CLIOption{"verbose", "Enables verbose
/// output"}.MetaVar("level").Short('v').Long("verbose").Flag().Repeated()
/// \encode
///
/// Compared to the previous snippet, this also enables --verbose as a synonym
/// for -v, shows `-v/--verbose <level>` instead of tautological `-v/--verbose
/// <verbosse>` in help, and allows repetition of the option. Therefore, the
/// option is no longer passed as a boolean, but as an integer. For example,
/// `-vvv` resolves to 3.
///
/// Other methods include the `With*Argument*()` family to allow options to
/// receive arguments, and Required() to necessiate that the option is passed.
///
/// The C++ type of the parsed option is stored in `ValueType` and depends on
/// the methods called. For example, in the previous two examples, Flag() yields
/// `bool`, while `Flag().Repeated()` yields `size_t`.
template <bool HasExplicitKey = false, bool HasExplicitMetaVar = false,
          CLIOptionKind ArgumentKind = CLIOptionKind::UNKNOWN, bool IsRequired = false, bool IsRepeated = false,
          typename MapFunctions = std::tuple<>>
class CLIOption {
protected:
    template <bool OtherHasExplicitKey, bool OtherHasExplicitMetaVar, CLIOptionKind OtherArgumentKind,
              bool OtherIsRequired, bool OtherIsRepeated, typename OtherMapFunctions>
    friend class CLIOption;

    CLIOption(std::string_view name, std::string_view description, std::string_view meta_var,
              std::vector<char> short_keys, std::vector<std::string_view> long_keys, size_t min_arguments,
              size_t max_arguments, MapFunctions map_functions)
        : name(name),
          description(description),
          meta_var(meta_var),
          short_keys(std::move(short_keys)),
          long_keys(std::move(long_keys)),
          min_arguments(min_arguments),
          max_arguments(max_arguments),
          map_functions(std::move(map_functions)) {
    }

public:
    std::string_view name;
    std::string_view description;
    std::string_view meta_var;
    std::vector<char> short_keys;
    std::vector<std::string_view> long_keys;
    size_t min_arguments;
    size_t max_arguments;
    MapFunctions map_functions;

    static constexpr bool ExplicitKey = HasExplicitKey;
    static constexpr CLIOptionKind Kind = ArgumentKind;
    static constexpr bool IsOptionRequired = IsRequired;
    static constexpr bool IsOptionRepeated = IsRepeated;

    /// The type of the value an option if it was used exactly once, e.g. `--name
    /// arg1 arg2` would be stored as `vector{"arg1", "arg2"}`.
    ///
    /// This is computed as follows:
    ///
    /// - If the option is Flag(), this is `std::monostate`, as the option has no
    /// argument and is a singleton.
    /// - If the option is WithArgument(), this is a single `std::string_view`.
    /// - If the option is WithOptionalArgument(), this is
    /// `std::optional<std::string_view>` (empty if the option is used without an
    /// argument following it immediately).
    /// - If the option is `With*Arguments()`, this is
    /// `std::vector<std::string_view>`.
    using SingleOptionType = std::conditional_t<
        ArgumentKind == CLIOptionKind::FLAG, std::monostate,
        std::conditional_t<ArgumentKind == CLIOptionKind::SINGLE_VALUE, std::string_view,
                           std::conditional_t<ArgumentKind == CLIOptionKind::OPTIONAL_SINGLE_VALUE,
                                              std::optional<std::string_view>, std::vector<std::string_view>>>>;

    /// The type of a repeated group of the same options.
    using MultipleOptionType = std::vector<SingleOptionType>;

    /// The resulting type of the parsed option before Map() is applied.
    ///
    /// This is computed as follows:
    ///
    /// Unless the option is a Flag():
    ///
    /// - If the option is required but not repeated, i.e. expected to appear
    /// exactly once, this is equal to `SingleOptionType`.
    /// - If the option is neither required nor repeated, i.e. may either appear
    /// once or be ommitted, this is equal to `std::optional<SingleOptionType>`.
    /// - If the option is repeated, this is `std::vector<SingleOptionType>`.
    ///
    /// If the option is a Flag(), the logic is as follows:
    ///
    /// - If the option is required but not repeated (an extermely odd case), this
    /// is `std::monostate`.
    /// - If the option is neither required nor repeated, this is `bool`.
    /// - If the option is repeated, this is `size_t`.
    ///
    /// Note that this may result in odd and similar types like
    /// `std::vector<std::optional<std::string_view>>` or
    /// `std::optional<std::vector<std::string_view>>`. This is intended. The
    /// former would be used for argument strings like `--opt value --opt`,
    /// parsing as `{std::string_view{"value"}, std::nullopt}`. The latter would
    /// be used for argument strings like `--opt value1 value2` and `--opt` (which
    /// is different from `--opt` missing at all).
    using OriginalType = std::conditional_t<
        ArgumentKind == CLIOptionKind::FLAG,
        std::conditional_t<IsRepeated, size_t, std::conditional_t<IsRequired, std::monostate, bool>>,
        std::conditional_t<IsRepeated, std::vector<SingleOptionType>,
                           std::conditional_t<IsRequired, SingleOptionType, std::optional<SingleOptionType>>>>;

    /// The resulting type of the parsed option.
    ///
    /// If you do not specify Map() for this option, this is equivalent to
    /// ::OriginalType, which we shall refer the user to. If Map() is specified,
    /// this is the return type of the mapping function(s).
    using ValueType = decltype(MapFunctionComposition(map_functions, std::declval<OriginalType>()));

    /// Initializes a CLI option configuration.
    /// \param name A short unique name of the option, visible to the end user,
    /// e.g. `verbose`, `output`, or `help`. \param description Information about
    /// the option visible in help.
    explicit CLIOption(std::string_view name, std::string_view description = "")
        : name(name), description(description), meta_var(name) {
    }

    /// Specifies the "units", or the value type of the arguments of the option.
    /// For example, the metavar of the GCC
    /// `-o` option would be "outfile", and the metavar of `-O` would be "level"
    /// or "opt-level".
    CLIOption<HasExplicitKey, true, ArgumentKind, IsRequired, IsRepeated, MapFunctions> MetaVar(std::string_view text) {
        static_assert(!HasExplicitMetaVar,
                      "This MetaVar() call overrides the "
                      "previous MetaVar() on the same option");
        return {name,
                description,
                text,
                std::move(short_keys),
                std::move(long_keys),
                min_arguments,
                max_arguments,
                std::move(map_functions)};
    }

    /// Assigns a short key, e.g. `-v`, to the option.
    ///
    /// Code example:
    ///
    /// \code{cpp}
    /// CLIOption{"verbose", "Set verbosity level"}.Short('v')
    /// \endcode
    ///
    /// An option may be assigned several short keys, and long keys too. If an
    /// option is assigned neither a short key nor a long key, it acts as a
    /// catch-all option for all arguments listed without appropriate options.
    ///
    /// A short option may be listed either as an isolated `-<character>` option
    /// in the command line, or joined with other short options, e.g. `-abc` is
    /// synonymous with `-a -b -c`, both enabling three options associated with
    /// short keys `a`, `b`, and `c`.
    ///
    /// If the option takes an argument or several arguments, the first argument
    /// may either be passed like `-<character> <argument>`, or concatenated to
    /// the key, e.g. `-<character><argument>`. If the option takes exactly one
    /// optional argument, it must be passed as `-<character><argument>`. Yes,
    /// this prevents passing an empty argument.
    ///
    /// Examples for better understanding:
    ///
    /// - If `-a`, `-b`, `-c` are flags:
    ///   - `-abc` sets three boolean flags
    ///   - `-a -b -c` sets three boolean flags
    ///   - `-a -bc` sets three boolean flags
    /// - If `-a` is a flag, and `-b` takes exactly one argument:
    ///   - `-abc` sets one boolean flag (`-a`) and sets the value of `-b` to
    ///   `"c"`.
    ///   - `-a -bc` sets one boolean flag (`-a`) and sets the value of `-b` to
    ///   `"c"`.
    ///   - `-a -b c` sets one boolean flag (`-a`) and sets the value of `-b` to
    ///   `"c"`.
    ///   - `-a -b -c` sets one boolean flag (`-a`) and sets the value of `-b` to
    ///   `"-c"`. This slightly counterintuitive
    ///     detail allows one to use values starting with dashes.
    /// - If `-a` is a flag, and `-b` takes exactly one optional argument:
    ///   - `-abc` and `-a -bc` set one boolean flag (`-a`) and sets the value of
    ///   `-b` to `"c"`.
    ///   - `-a -b` sets one boolean flag (`-a`) and sets `-b` to `std::nullopt`.
    ///   - `-a -b c` sets one boolean flag (`-a`), sets `-b` to `std::nullopt`,
    ///   and parses `c` as an argument to an unnamed option.
    ///   - If `-c` is a flag, `-a -b -c` sets two boolean flags (`-a` and `-c`)
    ///   and sets `-b` to `std::nullopt`.
    ///   - `-a -b-c` sets one boolean flag (`-a`) and and sets `-b` to `"-c"`.
    /// - If `-a` is a flag, and `-b` takes several arguments (whether they are
    /// required or optional is of no
    ///   importance):
    ///   - `-abc` sets one boolean flag (`-a`) and sets the value of `-b` to
    ///   `{"c"}`.
    ///   - `-abtext` sets one boolean flag (`-a`) and sets the value of `-b` to
    ///   `{"text"}`.
    ///   - `-ab arg1 arg2` sets one boolean flag (`-a`) and sets the value of
    ///   `-b` to `{"arg1", "arg2"}`.
    ///   - `-abarg1 arg2` sets one boolean flag (`-a`) and sets the value of `-b`
    ///   to `{"arg1", "arg2"}`.
    CLIOption<true, HasExplicitMetaVar, ArgumentKind, IsRequired, IsRepeated, MapFunctions> Short(char short_key) {
        short_keys.push_back(short_key);
        return {name,
                description,
                meta_var,
                std::move(short_keys),
                std::move(long_keys),
                min_arguments,
                max_arguments,
                std::move(map_functions)};
    }

    /// Assigns a long key, e.g. `--verbose`, to the option. The `--` prefix is
    /// implicit and must not be listed.
    ///
    /// Code example:
    ///
    /// \code{cpp}
    /// CLIOption{"verbose", "Set verbosity level"}.Long("verbose")
    /// \endcode
    ///
    /// An option may be assigned several long keys, and short keys too. If an
    /// option is assigned neither a short key nor a long key, it acts as a
    /// catch-all option for all arguments listed without appropriate options.
    ///
    /// A long option is listed as an isolated `--<key>` option in the command
    /// line.
    ///
    /// If the option takes an argument or several arguments, the first argument
    /// may either be passed like `--<key> <argument>`, or joined to the key via
    /// an equals sign, e.g. `--<key>=<argument>`. If the option takes exactly one
    /// optional argument, it must be passed as `--<key>=<argument>`.
    ///
    /// Examples for better understanding:
    ///
    /// - If `--foo` and `--bar` are flags:
    ///   - `--foo --bar` sets two boolean flags
    /// - If `--foo` takes exactly one argument:
    ///   - `--foo=bar` sets the value of `--foo` to `"bar"`.
    ///   - `--foo bar` sets the value of `--foo` to `"bar"`.
    ///   - `--foo=--bar` sets the value of `--foo` to `"--bar"`.
    ///   - `--foo --bar` sets the value of `--foo` to `"--bar"`. This slightly
    ///   counterintuitive detail allows one to
    ///     use values starting with dashes.
    /// - If `--foo` takes exactly one optional argument:
    ///   - `--foo=bar` sets `--foo` to `"bar"`.
    ///   - `--foo=--bar` sets `--foo` to `"--bar"`.
    ///   - `--foo` sets `--foo` to `std::nullopt`.
    ///   - `--foo bar` sets `--foo` to `std::nullopt` and parses `bar` as an
    ///   argument to an unnamed option.
    ///   - If `--bar` is a flag, `--foo --bar` sets `--foo` to `std::nullopt` and
    ///   sets `--bar`.
    /// - If `--foo` takes several arguments (whether they are required or
    /// optional is of no importance):
    ///   - `--foo=text` sets `--foo` to `{"text"}`.
    ///   - `--foo arg1 arg2` sets `--foo` to `{"arg1", "arg2"}`.
    ///   - `--foo=arg1 arg2` sets `--foo` to `{"arg1", "arg2"}`.
    CLIOption<true, HasExplicitMetaVar, ArgumentKind, IsRequired, IsRepeated, MapFunctions> Long(
        std::string_view long_key) {
        long_keys.push_back(long_key);
        return {name,
                description,
                meta_var,
                std::move(short_keys),
                std::move(long_keys),
                min_arguments,
                max_arguments,
                std::move(map_functions)};
    }

    /// Specifies that the option must always be present.
    ///
    /// This changes the type of the value from `std::optional<T>` to `T`, unless
    /// this option is also Repeated(), in which case the type stays the same.
    CLIOption<HasExplicitKey, HasExplicitMetaVar, ArgumentKind, true, IsRepeated, MapFunctions> Required() {
        static_assert(!IsRequired, "Calling Required() twice on a single argument is redundant");
        return {name,
                description,
                meta_var,
                std::move(short_keys),
                std::move(long_keys),
                min_arguments,
                max_arguments,
                std::move(map_functions)};
    }

    /// Specifies that the option may be listed several times.
    ///
    /// This changes the type of the value from `std::optional<T>` or `T` to
    /// `std::vector<T>`.
    CLIOption<HasExplicitKey, HasExplicitMetaVar, ArgumentKind, IsRequired, true, MapFunctions> Repeated() {
        static_assert(!IsRepeated, "Calling Repeated() twice on a single argument is redundant");
        return {name,
                description,
                meta_var,
                std::move(short_keys),
                std::move(long_keys),
                min_arguments,
                max_arguments,
                std::move(map_functions)};
    }

    /// Specifies that the option is a flag, that is, it does not take any
    /// arguments.
    ///
    /// The type of the value of such an option is typically `bool`. If the option
    /// is Repeated(), the type is `size_t`, counting the number of occurences. If
    /// the option is Required() and not Repeated(), the option is always expected
    /// to be passed, making the value effectively useless except for
    /// compatibility, and the type `std::monostate`.
    CLIOption<HasExplicitKey, HasExplicitMetaVar, CLIOptionKind::FLAG, IsRequired, IsRepeated, MapFunctions> Flag() {
        static_assert(ArgumentKind == CLIOptionKind::UNKNOWN,
                      "Exactly one of Flag() and With*Argument*() must be used per option");
        return {
            name, description, meta_var, std::move(short_keys), std::move(long_keys), 0, 0, std::move(map_functions)};
    }

    /// Specifies that the option takes exactly one argument.
    ///
    /// The type of the value of such an option is typically
    /// `std::optional<std::string_view>`. If the option is Repeated(), the type
    /// is `std::vector<std::string_view>`, one element per occurence of the
    /// option. If the option is Required() and not Repeated(), the type is
    /// `std::string_view`.
    CLIOption<HasExplicitKey, HasExplicitMetaVar, CLIOptionKind::SINGLE_VALUE, IsRequired, IsRepeated, MapFunctions>
    WithArgument() {
        static_assert(ArgumentKind == CLIOptionKind::UNKNOWN,
                      "Exactly one of Flag() and With*Argument*() must be used per option");
        return {
            name, description, meta_var, std::move(short_keys), std::move(long_keys), 1, 1, std::move(map_functions)};
    }

    /// Specifies that the option takes one or zero arguments.
    ///
    /// The type of the value of such an option is typically
    /// `std::optional<std::optional<std::string_view>>`: the outer
    /// `std::optional` indicates whether the option is used, and the internal
    /// `std::optional` indicates whether it has an argument. If the option is
    /// Repeated(), the type is `std::vector<std::optional<std::string_view>>`,
    /// one element per occurence of the option. If the option is Required() and
    /// not Repeated(), the type is `std::optional<std::string_view>`.
    CLIOption<HasExplicitKey, HasExplicitMetaVar, CLIOptionKind::OPTIONAL_SINGLE_VALUE, IsRequired, IsRepeated,
              MapFunctions>
    WithOptionalArgument() {
        static_assert(ArgumentKind == CLIOptionKind::UNKNOWN,
                      "Exactly one of Flag() and With*Argument*() must be used per option");
        return {
            name, description, meta_var, std::move(short_keys), std::move(long_keys), 0, 1, std::move(map_functions)};
    }

    /// Specifies that the option takes several arguments, usually one or more.
    ///
    /// The type of the value of such an option is typically
    /// `std::optional<std::vector<std::string_view>>`. The external
    /// `std::optional` designates whether the option is present, and if it is,
    /// the vector is guaranteed to not be non-empty. If the option is Repeated(),
    /// the type is `std::vector<std::vector<std::string_view>>`: the external
    /// `std::vector` enumerates options, while the internal `std::vector`
    /// enumerates arguments. If the option is Required() and not Repeated(), the
    /// type is `std::vector<std::string_view>`, guaranteed to be non-empty.
    ///
    /// \param min The minimal number of options allowed. This may be zero, in
    /// which case the guarantees about non-emptyness of the vectors don't apply,
    /// or more than one. If you wish to set this to zero, consider
    /// WithOptionalArguments() instead.
    /// \param max The maximal number of options allowed
    /// \exception std::invalid_argument The provided bounds are invalid
    CLIOption<HasExplicitKey, HasExplicitMetaVar, CLIOptionKind::MULTIPLE_VALUES, IsRequired, IsRepeated, MapFunctions>
    WithArguments(size_t min = 1, size_t max = std::numeric_limits<size_t>::max()) {
        static_assert(ArgumentKind == CLIOptionKind::UNKNOWN,
                      "Exactly one of Flag() and With*Argument*() must be used per option");
        if (min > max) {
            throw std::invalid_argument(
                "Lower bound on the count of arguments of an "
                "option is higher than upper bound");
        }
        return {name,
                description,
                meta_var,
                std::move(short_keys),
                std::move(long_keys),
                min,
                max,
                std::move(map_functions)};
    }

    /// Specifies that the option takes zero or more arguments.
    ///
    /// The type of the value of such an option is typically
    /// `std::optional<std::vector<std::string_view>>`. The external
    /// `std::optional` designates whether the option is present, not whether the
    /// option takes zero arguments. If the option is Repeated(), the type is
    /// `std::vector<std::vector<std::string_view>>`: the external `std::vector`
    /// enumerates options, while the internal `std::vector` enumerates arguments.
    /// If the option is Required() and not Repeated(), the type is
    /// `std::vector<std::string_view>`.
    ///
    /// \param max The maximal number of options allowed
    auto WithOptionalArguments(size_t max = std::numeric_limits<size_t>::max()) {
        return WithArguments(0, max);
    }

    /// Pass parsed data through an arbitrary transformer function.
    ///
    /// After parsing the option into strings, vectors of strings, or other data
    /// structures (see ::OriginalType), pass the data through a transformer
    /// function, e.g. to parse strings into integers. The transformer is invoked
    /// once, not per argument.
    ///
    /// If Map() is called on a single argument several times, this is equivalent
    /// to calling Map() once with a composition of the functions:
    ///
    /// For example, the following code will append `"12"` to the argument,
    /// converting it from std::string_view to std::string in the process:
    ///
    /// \code{.cpp}
    /// ...
    ///     .Map([](std::string_view s) { return s + "1"; })
    ///     .Map([](std::string s) { return s + "2"; })
    /// \endcode
    ///
    /// \param fn The transformer function. If this is the first transformer, it
    /// must accept a single argument of type
    /// ::OriginalType. The returned type is arbitrary.
    template <typename Fn>
    auto Map(Fn fn) -> CLIOption<HasExplicitKey, HasExplicitMetaVar, ArgumentKind, IsRequired, IsRepeated,
                                 decltype(std::tuple_cat(map_functions, std::tuple{fn}))> {
        return {name,
                description,
                meta_var,
                std::move(short_keys),
                std::move(long_keys),
                min_arguments,
                max_arguments,
                std::tuple_cat(map_functions, std::tuple{fn})};
    }

    /// Parse arguments into integers, booleans, floating-point types, or
    /// user-defined types.
    ///
    /// `As<T>()` is equivalent to `Map(RecursiveStringParser<T>{})`.
    ///
    /// Example:
    ///
    /// \code{.cpp}
    /// CLIOption{"optimization", "Set optimization level"}.MetaVar("opt-level").Short('O').As<int>()
    /// \endcode
    template <typename ElementType>
    auto As() {
        return Map(RecursiveStringParser<ElementType>{});
    }

    /// Returns a short user-friendly string designating the option in error
    /// messages, including both the name of the option and its keys.
    std::string GetUserFriendlyName() const {
        std::string text = "\"";
        text += name;
        text += '"';
        if (short_keys.empty() && long_keys.empty()) {
            text += " (unnamed)";
        } else {
            text += " (";
            bool is_first_item = true;
            for (char c : short_keys) {
                if (is_first_item) {
                    is_first_item = false;
                } else {
                    text += '/';
                }
                text += '-';
                text += c;
            }
            for (std::string_view key : long_keys) {
                if (is_first_item) {
                    is_first_item = false;
                } else {
                    text += '/';
                }
                text += "--";
                text += key;
            }
            text += ')';
        }
        return text;
    }

    /// Print short details about the option to an output stream.
    /// \param out The stream to write the details too.
    void ShowOneLineOptionHelp(std::ostream& out = std::cerr) const {
        std::string text;

        // Keys
        for (char c : short_keys) {
            if (!text.empty()) {
                text += '/';
            }
            text += '-';
            text += c;
        }
        for (std::string_view key : long_keys) {
            if (!text.empty()) {
                text += '/';
            }
            text += "--";
            text += key;
        }

        // Values
        if (max_arguments > 0) {
            if (!text.empty()) {
                if (Kind == CLIOptionKind::OPTIONAL_SINGLE_VALUE) {
                    if (!long_keys.empty()) {
                        text += '=';
                    }
                } else {
                    text += ' ';
                }
            }
            if (min_arguments == 0) {
                text += '[';
            }
            text += '<';
            text += meta_var;
            text += '>';
            if (max_arguments > 1) {
                text += "...";
            }
            if (min_arguments == 0) {
                text += ']';
            }
        }

        out << text;
    }

    /// Print details about the option to an output stream.
    /// \param out The stream to write the details too.
    void ShowOptionHelp(std::ostream& out = std::cerr) const {
        static const size_t OPTION_COLUMN_WIDTH = 20;

        std::ostringstream ss;
        ShowOneLineOptionHelp(ss);
        auto text = ss.str();

        if (description.empty()) {
            out << text << std::endl;
            return;
        }

        if (text.size() < OPTION_COLUMN_WIDTH) {
            text += std::string(OPTION_COLUMN_WIDTH - text.size(), ' ');
            out << text << description << std::endl;
        } else {
            out << text << std::endl << std::string(OPTION_COLUMN_WIDTH, ' ') << description << std::endl;
        }
    }
};

/// Universal parametrized command line parser.
///
/// To create the parser, pass it a list of options, e.g. as follows:
///
/// \code{.cpp}
/// CLIOptionParser parser{
///     CLIOption{"verbose", "Enables verbose output"}.Short('v').Long("verbose").Flag(),
///     CLIOption{"help", "Show help"}.Short('h').long("help").Flag(),
///     CLIOption{"needle", "Regular expression to search for"}.MetaVar("regex").WithArgument(),
///     CLIOption{"input", "Paths to files"}.MetaVar("file").WithArguments()
/// };
/// \endcode
///
/// ...and then parse args as follows:
///
/// \code{.cpp}
/// // int main(int argc, char** argv) {
/// auto [verbose, help, needle, input] = parser.Parse(argv + 1);
/// \endcode
template <typename... CLIOptions>
class CLIOptionParser {
protected:
    /// Check whether the argument is a start of an option or of another special
    /// kind
    bool IsSpecialArgument(std::string_view argument) const {
        // -short, --long, --
        return argument.size() >= 2 && argument[0] == '-';
    }

    /// Parse a vector of arguments of a single option.
    /// \exception CLIArgumentParsingError The provided arguments are invalid,
    /// e.g. there are too many of them.
    template <typename Option>
    void HandleOption(const Option& option, typename Option::MultipleOptionType& value,
                      std::vector<std::string_view> arguments) const {
        if (option.min_arguments == option.max_arguments) {
            if (arguments.size() != option.min_arguments) {
                throw CLIArgumentParsingError("Expected exactly "s + std::to_string(option.min_arguments) +
                                              " argument(s) for option " + option.GetUserFriendlyName() + ", got " +
                                              std::to_string(arguments.size()) + '.');
            }
        } else {
            if (arguments.size() < option.min_arguments) {
                throw CLIArgumentParsingError("Expected at least "s + std::to_string(option.min_arguments) +
                                              " argument(s) for option " + option.GetUserFriendlyName() + ", got " +
                                              std::to_string(arguments.size()) + '.');
            } else if (arguments.size() > option.max_arguments) {
                throw CLIArgumentParsingError("Expected at most "s + std::to_string(option.max_arguments) +
                                              " argument(s) for option " + option.GetUserFriendlyName() + ", got " +
                                              std::to_string(arguments.size()) + '.');
            }
        }
        if constexpr (Option::Kind == CLIOptionKind::FLAG) {
            value.emplace_back(std::monostate{});
        } else if constexpr (Option::Kind == CLIOptionKind::SINGLE_VALUE) {
            value.emplace_back(arguments[0]);
        } else if constexpr (Option::Kind == CLIOptionKind::OPTIONAL_SINGLE_VALUE) {
            value.emplace_back(arguments.empty() ? std::nullopt : std::optional{arguments[0]});
        } else {
            value.emplace_back(std::move(arguments));
        }
    }

    /// Parse a vector of arguments of an option with no short or long key.
    /// \exception CLIArgumentParsingError The provided arguments are invalid,
    /// e.g. no rest arguments are expected.
    template <size_t... I>
    void ParseRestArguments(const std::vector<std::string_view>& arguments,
                            std::tuple<typename CLIOptions::MultipleOptionType...>& result,
                            std::index_sequence<I...>) const {
        size_t arguments_offset = 0;
        (ParseRestOption(std::get<I>(options_), arguments, arguments_offset, std::get<I>(result)), ...);
        if (arguments_offset < arguments.size()) {
            throw CLIArgumentParsingError("Argument "s + arguments[arguments_offset].data() +
                                          " is not attached to a named option nor an unnamed option.");
        }
    }

    /// Parses some prefix of a vector of rest arguments of a particular option,
    /// if it has no short or long key. \exception CLIArgumentParsingError The
    /// provided arguments are invalid, e.g. there are too many of them.
    template <typename Option>
    void ParseRestOption(const Option& option, const std::vector<std::string_view>& arguments, size_t& arguments_offset,
                         typename Option::MultipleOptionType& value) const {
        if (!(option.short_keys.empty() && option.long_keys.empty())) {
            return;
        }
        size_t cnt_arguments = std::min(arguments.size() - arguments_offset, option.max_arguments);
        HandleOption(option, value,
                     {arguments.begin() + static_cast<ptrdiff_t>(arguments_offset),
                      arguments.begin() + static_cast<ptrdiff_t>(arguments_offset + cnt_arguments)});
        arguments_offset += cnt_arguments;
    }

    /// Parse a long option starting at a given position.
    /// \exception CLIArgumentParsingError The provided arguments are invalid,
    /// e.g. the long option is not supported.
    template <size_t... I>
    void ParseLongArgument(const char* const*& argv, std::string_view argument,
                           std::tuple<typename CLIOptions::MultipleOptionType...>& result,
                           std::index_sequence<I...>) const {
        std::string_view name;
        std::optional<std::string_view> inline_value;
        auto equals_sign_pos = argument.find('=');
        if (equals_sign_pos == std::string_view::npos) {
            name = argument.substr(2);
        } else {
            name = argument.substr(2, equals_sign_pos - 2);
            inline_value = argument.substr(equals_sign_pos + 1);
        }
        bool success =
            (TryParseLongOption(std::get<I>(options_), argv, name, inline_value, std::get<I>(result)) || ...);
        if (!success) {
            throw CLIArgumentParsingError("Unknown long option --"s + name.data() + '.');
        }
    }

    /// Attempt to parse a particular long option.
    /// \exception CLIArgumentParsingError The provided arguments are invalid,
    /// e.g. there are too many of them. \returns Whether the given option took up
    /// parsing the arguments.
    template <typename Option>
    bool TryParseLongOption(const Option& option, const char* const*& argv, std::string_view name,
                            std::optional<std::string_view> inline_value,
                            typename Option::MultipleOptionType& value) const {
        if (std::find(option.long_keys.begin(), option.long_keys.end(), name) == option.long_keys.end()) {
            return false;
        }
        std::vector<std::string_view> arguments;
        if (inline_value) {
            arguments.emplace_back(*inline_value);
        }
        if (Option::Kind != CLIOptionKind::OPTIONAL_SINGLE_VALUE) {
            while (arguments.size() < option.max_arguments && *argv &&
                   (!((*argv)[0] == '-' && (*argv)[1] != '\0') || arguments.size() < option.min_arguments)) {
                arguments.emplace_back(*argv++);
            }
        }
        HandleOption(option, value, std::move(arguments));
        return true;
    }

    /// Parse a short option starting at a given position.
    /// \exception CLIArgumentParsingError The provided arguments are invalid,
    /// e.g. the short option is not supported.
    template <size_t... I>
    void ParseShortArgument(const char* const*& argv, std::string_view argument,
                            std::tuple<typename CLIOptions::MultipleOptionType...>& result,
                            std::index_sequence<I...>) const {
        size_t offset = 1;
        while (offset < argument.size()) {
            char character = argument[offset];
            ++offset;
            bool success =
                (TryParseShortOption(std::get<I>(options_), argv, character, argument, offset, std::get<I>(result)) ||
                 ...);
            if (!success) {
                throw CLIArgumentParsingError("Unknown short option -"s + character + '.');
            }
        }
    }

    /// Attempt to parse a particular short option.
    /// \exception CLIArgumentParsingError The provided arguments are invalid,
    /// e.g. there are too many of them. \returns Whether the given option took up
    /// parsing the arguments.
    template <typename Option>
    bool TryParseShortOption(const Option& option, const char* const*& argv, char character, std::string_view argument,
                             size_t& offset, typename Option::MultipleOptionType& value) const {
        if (std::find(option.short_keys.begin(), option.short_keys.end(), character) == option.short_keys.end()) {
            return false;
        }
        std::vector<std::string_view> arguments;
        if (option.max_arguments > 0 && offset < argument.size()) {
            arguments.emplace_back(argument.substr(offset));
            offset = argument.size();
        }
        if (Option::Kind != CLIOptionKind::OPTIONAL_SINGLE_VALUE) {
            while (arguments.size() < option.max_arguments && *argv &&
                   (*argv[0] != '-' || arguments.size() < option.min_arguments)) {
                arguments.emplace_back(*argv++);
            }
        }
        HandleOption(option, value, std::move(arguments));
        return true;
    }

    /// Convert intermediate representation to the expected result type.
    /// \exception CLIArgumentParsingError A required option is not present, a
    /// non-repeated option is present twice, or the option values are incorrect.
    /// \returns A tuple of parsed options.
    template <size_t... I>
    std::tuple<typename CLIOptions::ValueType...> FinalizeParsing(
        std::tuple<typename CLIOptions::MultipleOptionType...>& result, std::index_sequence<I...>) const {
        return std::make_tuple(FinalizeOptionParsing(std::get<I>(options_), std::get<I>(result))...);
    }

    /// Convert intermediate representation of a single option to the expected
    /// result type. \exception CLIArgumentParsingError A required option is not
    /// present, a non-repeated option is present twice, or the option values are
    /// incorrect. \returns The value of the parsed option.
    template <typename Option>
    typename Option::ValueType FinalizeOptionParsing(const Option& option,
                                                     typename Option::MultipleOptionType& value) const {
        if (Option::IsOptionRequired && value.empty()) {
            throw CLIArgumentParsingError("Missing required option "s + option.GetUserFriendlyName() + '.');
        }
        if (!Option::IsOptionRepeated && value.size() > 1) {
            throw CLIArgumentParsingError("Option "s + option.GetUserFriendlyName() + " can only be specified once.");
        }

        typename Option::OriginalType result;
        if constexpr (Option::Kind == CLIOptionKind::FLAG) {
            if constexpr (Option::IsOptionRepeated) {
                result = value.size();
            } else if constexpr (Option::IsOptionRequired) {
                result = std::monostate{};
            } else {
                result = !value.empty();
            }
        } else {
            if constexpr (Option::IsOptionRepeated) {
                result = value;
            } else if constexpr (Option::IsOptionRequired) {
                result = value[0];
            } else {
                result = value.empty() ? std::nullopt : std::optional{value[0]};
            }
        }

        return MapFunctionComposition(option.map_functions, result);
    }

public:
    /// Construct the parser from a compile-time-generated list of options and
    /// configuration.
    explicit CLIOptionParser(CLIOptions... options) : options_(std::move(options)...) {
        static_assert(((CLIOptions::Kind != CLIOptionKind::UNKNOWN) && ...),
                      "The kind of each option must be specified");
        static_assert(((CLIOptions::Kind != CLIOptionKind::FLAG || CLIOptions::ExplicitKey) && ...),
                      "Flag options must have -short/--long keys");
    }

    /// Parse a list of arguments.
    /// \exception CLIArgumentParsingError The given list of arguments does not
    /// satisfy option requirements. \returns A tuple of parsed options.
    std::tuple<typename CLIOptions::ValueType...> Parse(const char* const* argv) const {
        std::tuple<typename CLIOptions::MultipleOptionType...> parsed_values{};

        auto index_sequence = std::make_index_sequence<sizeof...(CLIOptions)>();

        std::vector<std::string_view> rest_arguments;
        while (*argv) {
            std::string_view argument = *argv++;
            if (!IsSpecialArgument(argument)) {
                // Simple literal
                rest_arguments.push_back(argument);
            } else if (argument == "--") {
                // Start of unnamed arguments
                break;
            } else if (argument.substr(0, 2) == "--") {
                // Long option: --name or --name=value
                ParseLongArgument(argv, argument, parsed_values, index_sequence);
            } else {
                // Short option(s)
                ParseShortArgument(argv, argument, parsed_values, index_sequence);
            }
        }

        if (!rest_arguments.empty()) {
            ParseRestArguments(rest_arguments, parsed_values, index_sequence);
        }

        return FinalizeParsing(parsed_values, index_sequence);
    }

    /// Shows a one-liner about supported options.
    ///
    /// If you find yourself calling this function directly, consider using
    /// ::CLIApplication instead.
    void ShowOneLineOptionsHelp(std::string_view program_path, std::ostream& stream = std::cerr) const {
        stream << program_path << " [options]";
        std::apply(
            [&](const CLIOptions&... options) {
                ((CLIOptions::ExplicitKey ? (void)0 : (stream << ' ', options.ShowOneLineOptionHelp(stream))), ...);
            },
            options_);
    }

    /// Shows information about supported options and their descriptions.
    ///
    /// If you find yourself calling this function directly, consider using
    /// ::CLIApplication instead.
    void ShowOptionsHelp(std::ostream& stream = std::cerr) const {
        std::apply([&](const CLIOptions&... options) { (options.ShowOptionHelp(stream), ...); }, options_);
    }

protected:
    std::tuple<CLIOptions...> options_;
};

/// A high-level interface for designing CLI applications.
///
/// This is a wrapper around a command line parser that automatically adds
/// `--help` and `--version` options, can handle parsing errors gracefully and
/// show help whenever required, and so on. Using this class should be preferred
/// to using
/// ::CLIOptionParser directly.
///
/// An instance of ::CLIApplication is constructed using MakeApplication() as
/// follows:
///
/// \code{.cpp}
/// auto app = clipper::MakeApplication(
///     "Program name",
///     "Program description",
///     "Program version",
///     clipper::CLIOption{"opt1"}...,
///     clipper::CLIOption{"opt2"}...,
///     ...
/// );
/// \endcode
///
/// As MakeApplication() enables `--help` and `--version` automatically, these
/// options must not be enabled manually.
///
/// After constructing the application object and adding logic checkers via
/// AddChecker() if necessary, command line arguments are to be parsed:
///
/// \code{.cpp}
/// // int main(int argc, char** argv) {
/// auto [opt1, opt2, ...] = app.StartArgv(argv);
/// \endcode
///
/// The values of `--help` and `--version` are not included in the returned
/// tuple, as StartArgv() handles them automatically. If a user error is
/// detected, help or a diagnostic message is printed, and StartArgv()
/// terminates the program.
template <typename CLIParser, typename... OriginalCLIOptions>
class CLIApplication {
protected:
    CLIApplication(std::string_view name, std::string_view description, std::string_view version, CLIParser parser)
        : name_(name), description_(description), version_(version), parser_(std::move(parser)), checkers_{} {
    }

public:
    /// Start an application with a command line, showing help and diagnostic
    /// messages if needed.
    ///
    /// When the input is invalid, this function does not throw an exception, but
    /// print a diagnostic message to the given stream, and then exits.
    ///
    /// \param argv The command line argument list
    /// \param stream Where to output errors, help, and version
    /// \param error_exit_code The process exit code to use on failure
    auto StartArgv(const char* const* argv, std::ostream& stream = std::cerr, int error_exit_code = 1) const {
        if (argv[0] == nullptr) {
            ShowHelp("<program>", stream);
            std::exit(error_exit_code);
        }
        try {
            return std::apply(
                [&](bool help_flag, bool version_flag, auto&&... rest) {
                    if (help_flag) {
                        ShowHelp(argv[0], stream);
                        std::exit(0);
                    }
                    if (version_flag) {
                        stream << name_ << " (version " << version_ << ")" << std::endl;
                        std::exit(0);
                    }
                    for (auto& checker : checkers_) {
                        checker(rest...);
                    }
                    return std::make_tuple(std::forward<decltype(rest)>(rest)...);
                },
                parser_.Parse(argv + 1));
        } catch (CLIArgumentParsingError& err) {
            if (argv[1] == nullptr) {
                ShowHelp(argv[0], stream);
                std::exit(0);
            } else {
                stream << err.what() << " Use --help for help." << std::endl;
                std::exit(error_exit_code);
            }
        }
    }

    /// Shows information about the usage of the program: its name, version,
    /// description, and options.
    ///
    /// \param program_path Path to executable, perhaps relative
    /// \param stream Where to output the details
    void ShowHelp(std::string_view program_path, std::ostream& stream = std::cerr) const {
        stream << name_ << " (version " << version_ << ")" << std::endl;
        stream << description_ << std::endl;
        stream << std::endl;
        stream << "Usage: ";
        parser_.ShowOneLineOptionsHelp(program_path);
        stream << std::endl;
        stream << std::endl;
        stream << "Options:" << std::endl;
        parser_.ShowOptionsHelp();
    }

    /// Adds a post-parsing checker that checks logical, rather than syntactic
    /// requirements.
    ///
    /// For example, if specifying one option should also require that another
    /// option is specified, this would be the right place to do so. The checker
    /// is a function that takes the entire parsed options list as arguments and
    /// throws ::CLIArgumentParsingError whenever it detects an error.
    ///
    /// Several checkers may be applied.
    ///
    /// Example:
    ///
    /// \code{.cpp}
    /// auto app = clipper::MakeApplication(
    ///     "Name",
    ///     "Description",
    ///     "1.0.0",
    ///     clipper::CLIOption{"opt1", "Option 1"}.Short('1').Flag(),
    ///     clipper::CLIOption{"opt2", "Option 2, requires option
    ///     1"}.Short('2').Flag()
    /// );
    /// app.AddChecker([](bool opt1, bool opt2) {
    ///     if(opt2 && !opt1) {
    ///         throw CLIArgumentParsingError("Option -2 is passed but -1 is
    ///         not.");
    ///     }
    /// });
    /// \endcode
    ///
    /// \param checker The checker function
    void AddChecker(std::function<void(const typename OriginalCLIOptions::ValueType&...)> checker) {
        checkers_.push_back(std::move(checker));
    }

protected:
    std::string_view name_;
    std::string_view description_;
    std::string_view version_;
    CLIParser parser_;
    std::vector<std::function<void(const typename OriginalCLIOptions::ValueType&...)>> checkers_;

    template <typename... CLIOptionsOther>
    friend auto MakeApplication(std::string_view name, std::string_view description, std::string_view version,
                                CLIOptionsOther&&... options);
};

/// Creates a high-level application object; see ::CLIApplication for more
/// information.
template <typename... CLIOptions>
auto MakeApplication(std::string_view name, std::string_view description, std::string_view version,
                     CLIOptions&&... options) {
    CLIOptionParser parser{CLIOption{"help", "Show usage information"}.Short('h').Long("help").Flag(),
                           CLIOption{"version", "Show version"}.Long("version").Flag(),
                           std::forward<CLIOptions>(options)...};
    return CLIApplication<decltype(parser), CLIOptions...>{name, description, version, parser};
};

}  // namespace clipper

#endif
