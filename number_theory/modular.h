#ifndef NUMBER_THEORY_MODULAR_H_
#define NUMBER_THEORY_MODULAR_H_

#include <stddef.h>

#include <bit>
#include <concepts>
#include <limits>
#include <type_traits>

#include "number_theory/utility.h"

// Modular arithmetic

namespace tql {
namespace number_theory {

namespace {

// Returns the equivalent element of |x| in the ring of integers modulo
// |modulus|. The result |y| should statisfy 0 <= y < modulus, and
// y = k*modulus + x for some integer k.
template <class T,
          std::enable_if_t<std::numeric_limits<T>::is_integer, bool> = true>
constexpr T normalize(T x, T modulus) {
  T y = std::move(x);
  if (y < 0 || y >= modulus) {
    y %= modulus;
    if (y < 0)
      y += modulus;
  }
  return y;
}

template <class T>
struct ModulusWrapper {
  using type = T;
  T value;
  constexpr ModulusWrapper(T x) : value(std::move(x)) {}
};

}  // namespace

template <ModulusWrapper mod>
class Modular {
 public:
  using type = std::decay_t<typename decltype(mod)::type>;
  static constexpr type modulus = mod.value;

  static_assert(std::numeric_limits<type>::is_integer && modulus > 0,
                "Modular requires modulus to be a positive integer.");

  // This is an implicit constructor. We implicitly upgrade from Modular::type
  // to Modular to make it easier to use.
  Modular(type value = 0) { set(std::move(value)); }

  Modular(const Modular &other) = default;
  Modular(Modular &&other) = default;
  Modular &operator=(const Modular &other) = default;
  Modular &operator=(Modular &&other) = default;

  // This is an implicit conversion, because we want a seamless conversion from
  // Modular to Modular::type.
  operator type() const { return get(); }

  const type &get() const { return value_; }

  void set(type value) { value_ = normalize(std::move(value), modulus); }

  Modular add(const Modular &rhs) const {
    check_addition_overflow();
    type new_value = value_ + rhs.value_;
    if (new_value >= modulus)
      new_value -= modulus;
    return Modular(new_value);
  }

  Modular negate() const {
    if (value_ == 0)
      return *this;
    return Modular(modulus - value_);
  }

  Modular substract(const Modular &rhs) const { return add(rhs.negate()); }

  Modular multiply(const Modular &rhs) const {
    check_multiplication_overflow();
    return Modular(value_ * rhs.value_);
  }

  bool equal(const Modular &rhs) const { return value_ == rhs.value_; }

 protected:
  type value_;

  static constexpr size_t type_width = std::numeric_limits<type>::digits;
  static constexpr size_t modulus_width =
      std::bit_width(static_cast<std::make_unsigned_t<type>>(modulus));

  void check_addition_overflow() const {
    static_assert(
        modulus_width + 1 <= type_width,
        "Modular addition may overflow. Please use larger integer types.");
  }

  void check_multiplication_overflow() const {
    static_assert(modulus_width * 2 <= type_width,
                  "Modular multiplication may overflow. "
                  "Please use larger integer types.");
  }
};

namespace {

// Tests if a class T is derived from Modular.
template <class T>
struct IsModularImpl {
  template <auto mod,
            std::enable_if_t<std::derived_from<T, Modular<mod>>, bool> = true>
  static constexpr std::true_type test(const Modular<mod> *);

  static constexpr std::false_type test(...);

  static constexpr bool result = decltype(test(std::declval<T *>()))::value;
};

template <class T>
concept IsModular = IsModularImpl<T>::result;

}  // namespace

// Overloads an arithmetic operator with a class method
#define OVERLOAD_ARITHMETIC_OPERATOR(CONCEPT, OP, METHOD)    \
  template <CONCEPT M>                                       \
  M operator OP(const M &lhs, const M &rhs) {                \
    return lhs.METHOD(rhs);                                  \
  }                                                          \
  template <CONCEPT M>                                       \
  M operator OP(const M &lhs, const typename M::type &rhs) { \
    return lhs.METHOD(M(rhs));                               \
  }                                                          \
  template <CONCEPT M>                                       \
  M operator OP(const typename M::type &lhs, const M &rhs) { \
    return M(lhs).METHOD(rhs);                               \
  }

// Overloads an inplace operator with a class method
#define OVERLOAD_INPLACE_OPERATOR(CONCEPT, OP, METHOD) \
  template <CONCEPT M>                                 \
  M &operator OP(M &lhs, const M &rhs) {               \
    lhs = lhs.METHOD(rhs);                             \
    return lhs;                                        \
  }

OVERLOAD_ARITHMETIC_OPERATOR(IsModular, +, add)
OVERLOAD_ARITHMETIC_OPERATOR(IsModular, -, subtract)
OVERLOAD_ARITHMETIC_OPERATOR(IsModular, *, multiply)
OVERLOAD_ARITHMETIC_OPERATOR(IsModular, ==, equal)

OVERLOAD_INPLACE_OPERATOR(IsModular, +=, add)
OVERLOAD_INPLACE_OPERATOR(IsModular, -=, subtract)
OVERLOAD_INPLACE_OPERATOR(IsModular, *=, multiply)

#undef OVERLOAD_ARITHMETIC_OPERATOR
#undef OVERLOAD_UNPLACE_OPERATOR

}  // namespace number_theory

using number_theory::Modular;

}  // namespace tql

#endif  // NUMBER_THEORY_MODULAR_H_
