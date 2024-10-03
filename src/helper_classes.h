#include <cstddef>
#include <exception>
#include <type_traits>

template <typename... Ts>
class variant;

inline constexpr size_t variant_npos = -1;

template <size_t I, typename Variant>
struct variant_alternative;

template <typename First, typename... Rest>
struct variant_alternative<0, variant<First, Rest...>> : std::type_identity<First> {};

template <size_t I, typename First, typename... Rest>
  requires(I < sizeof...(Rest) + 1)
struct variant_alternative<I, variant<First, Rest...>> : variant_alternative<I - 1, variant<Rest...>> {};

template <size_t I, typename Variant>
struct variant_alternative<I, const Variant>
    : std::type_identity<const typename variant_alternative<I, Variant>::type> {};

template <size_t I, typename Variant>
using variant_alternative_t = typename variant_alternative<I, Variant>::type;

template <typename Variant>
struct variant_size;

template <typename... Ts>
struct variant_size<variant<Ts...>> : std::integral_constant<size_t, sizeof...(Ts)> {};

template <typename... Ts>
struct variant_size<const variant<Ts...>> : std::integral_constant<size_t, variant_size<variant<Ts...>>::value> {};

template <typename Variant>
inline constexpr size_t variant_size_v = variant_size<Variant>::value;

template <typename T>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

template <size_t I>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

struct bad_variant_access : std::exception {
  const char* what() const noexcept override {
    return "bad_variant_access";
  }
};
