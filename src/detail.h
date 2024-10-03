#include "helper_classes.h"

#include <array>
#include <compare>
#include <concepts>
#include <cstddef>
#include <functional>
#include <memory>
#include <type_traits>
#include <utility>

namespace detail {

template <typename... Ts>
concept move_constructible = (std::is_move_constructible_v<Ts> && ...);

template <typename... Ts>
concept trivial_move_constructible = move_constructible<Ts...> && (std::is_trivially_move_constructible_v<Ts> && ...);

template <typename... Ts>
concept copy_constructible = (std::is_copy_constructible_v<Ts> && ...);

template <typename... Ts>
concept trivial_copy_constructible = copy_constructible<Ts...> && (std::is_trivially_copy_constructible_v<Ts> && ...);

template <typename... Ts>
concept destructible = (std::is_destructible_v<Ts> && ...);

template <typename... Ts>
concept trivial_destructible = destructible<Ts...> && (std::is_trivially_destructible_v<Ts> && ...);

template <typename... Ts>
concept copy_assignable = copy_constructible<Ts...> && (std::is_copy_assignable_v<Ts> && ...);

template <typename... Ts>
concept trivial_copy_assignable = copy_assignable<Ts...> && trivial_destructible<Ts...> &&
                                  trivial_copy_constructible<Ts...> && (std::is_trivially_copy_assignable_v<Ts> && ...);

template <typename... Ts>
concept move_assignable = move_constructible<Ts...> && (std::is_move_assignable_v<Ts> && ...);

template <typename... Ts>
concept trivial_move_assignable = move_assignable<Ts...> && trivial_destructible<Ts...> &&
                                  trivial_move_constructible<Ts...> && (std::is_trivially_move_assignable_v<Ts> && ...);

template <typename T>
struct is_variant_s : std::false_type {};

template <typename... Ts>
struct is_variant_s<variant<Ts...>> : std::true_type {};

template <typename T>
concept is_variant = is_variant_s<std::remove_cvref_t<T>>::value;

template <typename... Ts>
union variant_storage {};

template <typename First, typename... Rest>
union variant_storage<First, Rest...> {
  First first;
  variant_storage<Rest...> rest;

  constexpr variant_storage() noexcept {}

  constexpr ~variant_storage()
    requires destructible<First, Rest...>
  {}

  ~variant_storage()
    requires trivial_destructible<First, Rest...>
  = default;

  template <size_t I, typename... Args>
  constexpr explicit variant_storage(in_place_index_t<I>, Args&&... args)
      : rest(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr explicit variant_storage(in_place_index_t<0>, Args&&... args) : first(std::forward<Args>(args)...) {}

  template <size_t I>
  constexpr decltype(auto) get(in_place_index_t<I>) noexcept {
    return rest.get(in_place_index<I - 1>);
  }

  template <size_t I>
  constexpr decltype(auto) get(in_place_index_t<I>) const noexcept {
    return rest.get(in_place_index<I - 1>);
  }

  constexpr First& get(in_place_index_t<0>) noexcept {
    return first;
  }

  constexpr const First& get(in_place_index_t<0>) const noexcept {
    return first;
  }
};

template <typename T, typename... Ts>
struct index_by_type;

template <typename T, typename... Ts>
struct index_by_type<T, T, Ts...> : std::integral_constant<size_t, 0> {};

template <typename T, typename U, typename... Ts>
struct index_by_type<T, U, Ts...> : std::integral_constant<size_t, 1 + index_by_type<T, Ts...>::value> {};

template <typename T, typename... Ts>
inline constexpr size_t index_by_type_v = index_by_type<T, Ts...>::value;

template <typename T, typename... Ts>
struct type_count : std::integral_constant<size_t, (0ull + ... + std::is_same_v<T, Ts>)> {};

template <typename T, typename... Ts>
inline constexpr bool type_is_unique_v = type_count<T, Ts...>::value == 1;

template <typename To, typename From>
concept valid_conversion = requires(From t) { std::array<To, 1>{std::forward<From>(t)}; };

template <typename To, typename From>
concept perfect_conversion =
    valid_conversion<To, From> && std::same_as<std::remove_cvref_t<To>, std::remove_cvref_t<From>>;

template <typename T, typename... Ts>
struct overload_base {
  using base_idx = std::integral_constant<size_t, index_by_type_v<T, Ts...>>;

  template <typename Tp>
    requires perfect_conversion<T, Tp>
  static constexpr base_idx f(Tp&&);

  template <typename Tp>
    requires valid_conversion<T, Tp>
  static constexpr base_idx f(Tp&&);
};

template <typename... Ts>
struct overload : overload_base<Ts, Ts...>... {
  using overload_base<Ts, Ts...>::f...;
};

template <typename T, typename... Ts>
  requires((valid_conversion<Ts, T>) || ...)
inline constexpr size_t best_overload_v = decltype(overload<Ts...>::f(std::declval<T>()))::value;

template <template <typename> typename cmp>
inline constexpr auto cmp_lambda = []<typename L, typename R>(L&& l, R&& r) -> bool {
  if constexpr (std::is_same_v<L, R>) {
    return cmp<L>{}(l, r);
  }
  return false;
};

} // namespace detail

namespace visit_impl {

template <typename T, size_t... Sizes>
struct n_dim_array {
  T t;

  constexpr T operator()() const noexcept {
    return t;
  }
};

template <typename T, size_t I, size_t... Sizes>
struct n_dim_array<T, I, Sizes...> {
  n_dim_array<T, Sizes...> arr[I];

  constexpr T operator()(size_t i, auto... indexes) const noexcept {
    if (i == variant_npos) {
      i = I - 1;
    }
    return arr[i](indexes...);
  }
};

template <size_t... I>
struct expand_first;

template <size_t I, size_t... Indexes>
struct expand_first<I, Indexes...> : std::type_identity<std::make_index_sequence<I - 1>> {};

template <typename Sequence>
struct append_npos;

template <size_t... I>
struct append_npos<std::index_sequence<I...>> : std::type_identity<std::index_sequence<I..., variant_npos>> {};

template <size_t... I>
struct expand_with_npos : append_npos<typename expand_first<I...>::type> {};

template <>
struct expand_with_npos<> : std::type_identity<std::index_sequence<>> {};

template <size_t... I>
using expand_with_npos_t = expand_with_npos<I...>::type;

template <typename Sequence, typename VisitorArgs, typename Sizes, typename F>
struct visit_table;

template <size_t... I, size_t... VisitorArgs, size_t Size, size_t... Sizes, typename F>
struct visit_table<std::index_sequence<I...>, std::index_sequence<VisitorArgs...>, std::index_sequence<Size, Sizes...>,
                   F>
    : std::integral_constant<n_dim_array<F, Size, Sizes...>,
                             n_dim_array<F, Size, Sizes...>{
                                 visit_table<expand_with_npos_t<Sizes...>, std::index_sequence<VisitorArgs..., I>,
                                             std::index_sequence<Sizes...>, F>::value...}> {};

template <size_t... VisitorArgs, typename Visitor, typename Result>
struct visit_table<std::index_sequence<>, std::index_sequence<VisitorArgs...>, std::index_sequence<>,
                   Result (*)(Visitor)>
    : std::integral_constant<n_dim_array<Result (*)(Visitor)>, n_dim_array<Result (*)(Visitor)>{[](Visitor visitor) {
                               return std::forward<Visitor>(visitor)(std::integral_constant<size_t, VisitorArgs>{}...);
                             }}> {};

template <size_t>
struct zero : std::integral_constant<size_t, 0> {};

template <typename Visitor, size_t... Sizes>
inline constexpr auto table =
    visit_table<expand_with_npos_t<Sizes...>, std::index_sequence<>, std::index_sequence<Sizes...>,
                std::invoke_result_t<Visitor, zero<Sizes>...> (*)(Visitor)>::value;

template <typename Visitor, typename... Variants>
constexpr decltype(auto) visit_index(Visitor&& visitor, Variants&&... variants) {
  return table<Visitor, (variant_size_v<std::remove_cvref_t<Variants>> + 1)...>(variants.index()...)(
      std::forward<Visitor>(visitor));
}

} // namespace visit_impl
