#pragma once

#include "detail.h"

#include <compare>

template <typename... Ts>
  requires(!(std::is_array_v<Ts> || ...) && !(std::is_reference_v<Ts> || ...) && !(std::is_void_v<Ts> || ...) &&
           sizeof...(Ts) != 0)
class variant<Ts...> {

public:
  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<variant_alternative_t<0, variant>>)
    requires std::is_default_constructible_v<variant_alternative_t<0, variant>>
      : variant(in_place_index<0>) {}

  // generic ctor
  template <typename T, size_t I = detail::best_overload_v<T, Ts...>, typename Tp = variant_alternative_t<I, variant>>
    requires(!std::is_same_v<std::remove_cvref_t<T>, variant>)
  constexpr variant(T&& t) noexcept(std::is_nothrow_constructible_v<Tp, T>)
      : _storage(in_place_index<I>, std::forward<T>(t)),
        _type_idx(I) {}

  // generic operator=
  template <typename T, size_t I = detail::best_overload_v<T, Ts...>, typename Tp = variant_alternative_t<I, variant>>
    requires(!std::is_same_v<std::remove_cvref_t<T>, variant>)
  constexpr variant& operator=(T&& t) noexcept(std::is_nothrow_assignable_v<Tp, T> &&
                                               std::is_nothrow_constructible_v<Tp, T>) {
    visit_impl::visit_index(
        [&](auto i) {
          if constexpr (i == I) {
            _storage.get(in_place_index<I>) = std::forward<T>(t);
          } else if constexpr (std::is_nothrow_constructible_v<Tp, T> || !std::is_nothrow_move_constructible_v<Tp>) {
            emplace<I>(std::forward<T>(t));
          } else {
            emplace<I>(Tp(std::forward<T>(t)));
          }
        },
        *this);
    return *this;
  }

  template <size_t I, typename... Args>
    requires std::constructible_from<variant_alternative_t<I, variant>, Args...>
  constexpr variant_alternative_t<I, variant>& emplace(Args&&... args) {
    clear();
    auto* res = std::construct_at(std::addressof(_storage.get(in_place_index<I>)), std::forward<Args>(args)...);
    _type_idx = I;
    return *res;
  }

  template <typename T, typename... Args>
    requires(std::constructible_from<T, Args...> && detail::type_is_unique_v<T, Ts...>)
  constexpr T& emplace(Args&&... args) {
    return emplace<detail::index_by_type_v<T, Ts...>>(std::forward<Args>(args)...);
  }

  constexpr variant(const variant&)
    requires detail::trivial_copy_constructible<Ts...>
  = default;

  constexpr variant(const variant& other)
    requires detail::copy_constructible<Ts...>
  {
    visit_impl::visit_index(
        [&](auto i) {
          if constexpr (i != variant_npos) {
            emplace<i>(get<i>(other));
          }
        },
        other);
  }

  constexpr variant(variant&&) noexcept((std::is_nothrow_move_constructible_v<Ts> && ...))
    requires detail::trivial_move_constructible<Ts...>
  = default;

  constexpr variant(variant&& other) noexcept((std::is_nothrow_move_constructible_v<Ts> && ...))
    requires detail::move_constructible<Ts...>
  {
    visit_impl::visit_index(
        [&](auto i) {
          if constexpr (i != variant_npos) {
            emplace<i>(std::move(get<i>(other)));
          }
        },
        other);
  }

  constexpr variant& operator=(const variant&)
    requires detail::trivial_copy_assignable<Ts...>
  = default;

  constexpr variant& operator=(const variant& other)
    requires detail::copy_assignable<Ts...>
  {
    visit_impl::visit_index(
        [&](auto lhs, auto rhs) {
          if constexpr (lhs == variant_npos && rhs == variant_npos) {
            return;
          } else if constexpr (rhs == variant_npos) {
            clear();
          } else if constexpr (lhs == rhs) {
            get<lhs>(*this) = get<rhs>(other);
          } else if constexpr (std::is_nothrow_copy_constructible_v<variant_alternative_t<rhs, variant>> ||
                               !std::is_nothrow_move_constructible_v<variant_alternative_t<rhs, variant>>) {
            emplace<rhs>(get<rhs>(other));
          } else {
            emplace<rhs>(variant_alternative_t<rhs, variant>(get<rhs>(other)));
          }
        },
        *this, other);
    return *this;
  }

  constexpr variant& operator=(variant&&)
    requires detail::trivial_move_assignable<Ts...>
  = default;

  constexpr variant& operator=(variant&& other) noexcept(
      ((std::is_nothrow_move_constructible_v<Ts> && std::is_nothrow_move_assignable_v<Ts>)&&...))
    requires detail::move_assignable<Ts...>
  {
    visit_impl::visit_index(
        [&](auto lhs, auto rhs) {
          if constexpr (lhs == variant_npos && rhs == variant_npos) {
            return;
          } else if constexpr (rhs == variant_npos) {
            clear();
          } else if constexpr (lhs == rhs) {
            get<lhs>(*this) = std::move(get<rhs>(other));
          } else {
            emplace<rhs>(std::move(get<rhs>(other)));
          }
        },
        *this, other);
    return *this;
  }

  template <size_t I, class... Args>
    requires std::constructible_from<variant_alternative_t<I, variant>, Args...>
  constexpr explicit variant(in_place_index_t<I>, Args&&... args)
      : _storage(in_place_index<I>, std::forward<Args>(args)...),
        _type_idx(I) {}

  template <typename T, class... Args>
    requires(std::constructible_from<T, Args...> && detail::type_is_unique_v<T, Ts...>)
  constexpr explicit variant(in_place_type_t<T>, Args&&... args)
      : variant(in_place_index<detail::index_by_type_v<T, Ts...>>, std::forward<Args>(args)...) {}

  constexpr void swap(variant& other) noexcept(
      ((std::is_nothrow_move_constructible_v<Ts> && std::is_nothrow_swappable_v<Ts>)&&...))
    requires(((std::is_swappable_v<Ts> && std::is_move_constructible_v<Ts>) && ...))
  {
    visit_impl::visit_index(
        [&](auto lhs, auto rhs) {
          if constexpr (lhs == variant_npos && rhs == variant_npos) {
            return;
          } else if constexpr (lhs == variant_npos) {
            emplace<rhs>(std::move(get<rhs>(other)));
          } else if constexpr (rhs == variant_npos) {
            other.swap(*this);
            return;
          } else if constexpr (lhs == rhs) {
            using std::swap;
            swap(get<lhs>(*this), get<rhs>(other));
          } else {
            variant_alternative_t<rhs, variant> tmp(std::move(get<rhs>(other)));
            other.emplace<lhs>(std::move(get<lhs>(*this)));
            emplace<rhs>(std::move(tmp));
          }
          this->_type_idx = rhs;
          other._type_idx = lhs;
        },
        *this, other);
  }

  friend constexpr void swap(variant& lhs, variant& rhs) noexcept(
      ((std::is_nothrow_move_constructible_v<Ts> && std::is_nothrow_swappable_v<Ts>)&&...))
    requires(((std::is_swappable_v<Ts> && std::is_move_constructible_v<Ts>) && ...))
  {
    lhs.swap(rhs);
  }

  constexpr ~variant()
    requires detail::destructible<Ts...>
  {
    clear();
  }

  ~variant()
    requires detail::trivial_destructible<Ts...>
  = default;

  constexpr size_t index() const noexcept {
    return _type_idx;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index() == variant_npos;
  }

  template <typename T>
    requires detail::type_is_unique_v<T, Ts...>
  friend constexpr bool holds_alternative(const variant& v) noexcept {
    return v.index() == detail::index_by_type_v<T, Ts...>;
  }

  template <size_t I>
  friend constexpr variant_alternative_t<I, variant>& get(variant& v) {
    if (v.index() != I) {
      throw bad_variant_access();
    }
    return v._storage.get(in_place_index<I>);
  }

  template <size_t I>
  friend constexpr variant_alternative_t<I, variant>&& get(variant&& v) {
    return std::move(get<I>(v));
  }

  template <size_t I>
  friend constexpr const variant_alternative_t<I, variant>& get(const variant& v) {
    return get<I>(const_cast<variant&>(v));
  }

  template <size_t I>
  friend constexpr const variant_alternative_t<I, variant>&& get(const variant&& v) {
    return std::move(get<I>(v));
  }

  template <typename T>
    requires detail::type_is_unique_v<T, Ts...>
  friend constexpr T& get(variant& v) {
    return get<detail::index_by_type_v<T, Ts...>>(v);
  }

  template <typename T>
  friend constexpr T&& get(variant&& v) {
    return std::move(get<T>(v));
  }

  template <typename T>
  friend constexpr const T& get(const variant& v) {
    return get<T>(const_cast<variant&>(v));
  }

  template <typename T>
  friend constexpr const T&& get(const variant&& v) {
    return std::move(get<T>(v));
  }

  template <size_t I>
  friend constexpr variant_alternative_t<I, variant>* get_if(variant* pv) noexcept {
    if (!pv || pv->index() != I) {
      return nullptr;
    }
    return std::addressof(get<I>(*pv));
  }

  template <size_t I>
  friend constexpr const variant_alternative_t<I, variant>* get_if(const variant* pv) noexcept {
    return get_if<I>(const_cast<variant*>(pv));
  }

  template <typename T>
    requires detail::type_is_unique_v<T, Ts...>
  friend constexpr T* get_if(variant* pv) noexcept {
    return get_if<detail::index_by_type_v<T, Ts...>>(pv);
  }

  template <typename T>
  friend constexpr const T* get_if(const variant* pv) noexcept {
    return get_if<T>(const_cast<variant*>(pv));
  }

  friend constexpr bool operator==(const variant& lhs, const variant& rhs) {
    if (lhs.index() != rhs.index()) {
      return false;
    } else if (lhs.valueless_by_exception()) {
      return true;
    }
    return visit(detail::cmp_lambda<std::equal_to>, lhs, rhs);
  }

  friend constexpr bool operator!=(const variant& lhs, const variant& rhs) {
    if (lhs.index() != rhs.index()) {
      return true;
    } else if (lhs.valueless_by_exception()) {
      return false;
    }
    return visit(detail::cmp_lambda<std::not_equal_to>, lhs, rhs);
  }

  friend constexpr bool operator<(const variant& lhs, const variant& rhs) {
    if (rhs.valueless_by_exception()) {
      return false;
    } else if (lhs.valueless_by_exception()) {
      return true;
    } else if (lhs.index() < rhs.index()) {
      return true;
    } else if (lhs.index() > rhs.index()) {
      return false;
    }
    return visit(detail::cmp_lambda<std::less>, lhs, rhs);
  }

  friend constexpr bool operator>(const variant& lhs, const variant& rhs) {
    if (lhs.valueless_by_exception()) {
      return false;
    } else if (rhs.valueless_by_exception()) {
      return true;
    } else if (lhs.index() > rhs.index()) {
      return true;
    } else if (lhs.index() < rhs.index()) {
      return false;
    }
    return visit(detail::cmp_lambda<std::greater>, lhs, rhs);
  }

  friend constexpr bool operator<=(const variant& lhs, const variant& rhs) {
    if (lhs.valueless_by_exception()) {
      return true;
    } else if (rhs.valueless_by_exception()) {
      return false;
    } else if (lhs.index() < rhs.index()) {
      return true;
    } else if (lhs.index() > rhs.index()) {
      return false;
    }
    return visit(detail::cmp_lambda<std::less_equal>, lhs, rhs);
  }

  friend constexpr bool operator>=(const variant& lhs, const variant& rhs) {
    if (rhs.valueless_by_exception()) {
      return true;
    } else if (lhs.valueless_by_exception()) {
      return false;
    } else if (lhs.index() > rhs.index()) {
      return true;
    } else if (lhs.index() < rhs.index()) {
      return false;
    }
    return visit(detail::cmp_lambda<std::greater_equal>, lhs, rhs);
  }

private:
  constexpr void clear() noexcept {
    if (!valueless_by_exception()) {
      visit([](auto& e) { std::destroy_at(std::addressof(e)); }, *this);
      _type_idx = variant_npos;
    }
  }

private:
  detail::variant_storage<Ts...> _storage;
  size_t _type_idx = variant_npos;
};

template <typename Visitor, detail::is_variant... Variants,
          typename R = std::invoke_result_t<Visitor, decltype(get<0>(std::declval<Variants>()))...>>
constexpr R visit(Visitor&& visitor, Variants&&... variants) {
  return visit<R>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

template <typename R, typename Visitor, detail::is_variant... Variants>
constexpr R visit(Visitor&& visitor, Variants&&... variants) {
  return visit_impl::visit_index(
      [&](auto... i) -> R {
        if constexpr (((i == variant_npos) || ...)) {
          throw bad_variant_access();
        } else {
          return std::forward<Visitor>(visitor)(get<i>(std::forward<Variants>(variants))...);
        }
      },
      std::forward<Variants>(variants)...);
}

template <typename... Ts, typename Ret = std::common_comparison_category_t<std::compare_three_way_result_t<Ts>...>>
  requires(std::three_way_comparable<Ts> && ...)
constexpr Ret operator<=>(const variant<Ts...>& lhs, const variant<Ts...>& rhs) {
  if (lhs.valueless_by_exception() && rhs.valueless_by_exception()) {
    return std::strong_ordering::equal;
  } else if (lhs.valueless_by_exception()) {
    return std::strong_ordering::less;
  } else if (rhs.valueless_by_exception()) {
    return std::strong_ordering::greater;
  } else if (lhs.index() != rhs.index()) {
    return lhs.index() <=> rhs.index();
  }
  return visit(
      []<typename L, typename R>(L&& l, R&& r) -> Ret {
        if constexpr (std::is_same_v<L, R>) {
          return l <=> r;
        }
        return std::strong_ordering::equal;
      },
      lhs, rhs);
}
