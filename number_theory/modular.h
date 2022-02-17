#ifndef NUMBER_THEORY_MODULAR_H_
#define NUMBER_THEORY_MODULAR_H_

#include <stddef.h>

#include <bit>
#include <concepts>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <type_traits>
#include <utility>

#include "number_theory/numeric.h"
#include "number_theory/utility.h"

// Modular arithmetic

namespace tql {
namespace number_theory {

namespace modular_internal {

// Returns the equivalent element of |x| in the ring of integers modulo
// |modulus|. The result |y| should statisfy 0 <= y < modulus, and
// y = k*modulus + x for some integer k.
template <typename T>
constexpr T normalize(T x, T modulus) {
  static_assert(std::numeric_limits<T>::is_integer,
                "normalize arguments must be integers.");
  T y = std::move(x);
  if (y < 0 || y >= modulus) {
    y %= modulus;
    if (y < 0)
      y += modulus;
  }
  return y;
}

// Wrapper of a modulus of type T.
// This is a helper class that wraps the template arguments for the Modular
// class, so that we can pass a single constant to the Modular template without
// specifying its type, for example Modular<10>.
template <typename T>
struct ModulusWrapper {
  using type = T;
  T value;
  constexpr ModulusWrapper(T x) : value(std::move(x)) {}
};

}  // namespace modular_internal

// Returns the modular inverse of |number| in modulo |modulus| if exists.
// Otherwise, throws domain_error exception.
template <typename T>
T inverse_mod(const T &number, const T &modulus) {
  static_assert(std::numeric_limits<T>::is_integer,
                "inverse_mod arguments must be integers.");
  using Signed_T = std::make_signed_t<T>;
  Signed_T num = numeric_cast<Signed_T>(number);
  Signed_T mod = numeric_cast<Signed_T>(modulus);
  if (mod <= 0)
    throw std::invalid_argument("The modulus must be positive.");
  auto [x, y] = exgcd(num, mod);
  // Note that this will not overflow. If T has b bits, x*n + y*mod should be
  // equal to k * 2^b + 1 and by definition of exgcd, k must be zero.
  if (x * num + y * mod != 1) {
    // TODO: Use std::format once it is supported to provide more detailed error
    // message.
    //
    // throw std::domain_error(std::format("The inverse of {} does not
    //   exist in modulo {}.", number, mod));
    throw std::domain_error("The modular inverse does not exist.");
  }
  return modular_internal::normalize(std::move(x), mod);
}

// Ring of integers modulo |mod|.
template <modular_internal::ModulusWrapper mod>
class Modular {
 public:
  // The base type of the values in the ring.
  using type = std::decay_t<typename decltype(mod)::type>;
  // The modulus of the modular ring.
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

  // Retrieves the value as Modular::type.
  const type &get() const { return value_; }

  // Sets the element to a given value.
  void set(type value) {
    value_ = modular_internal::normalize(std::move(value), modulus);
  }

  // Addition in the modular ring.
  Modular add(const Modular &rhs) const {
    check_addition_overflow();
    type new_value = value_ + rhs.value_;
    if (new_value >= modulus)
      new_value -= modulus;
    return Modular(new_value);
  }

  // Returns the additive inverse.
  Modular negate() const {
    if (value_ == 0)
      return *this;
    return Modular(modulus - value_);
  }

  // Subtraction in the modular ring.
  Modular subtract(const Modular &rhs) const { return add(rhs.negate()); }

  // Multiplication in the modular ring.
  Modular multiply(const Modular &rhs) const {
    check_multiplication_overflow();
    return Modular(value_ * rhs.value_);
  }

  // Division in the modular ring. Throws std::domain_error if the
  // multiplicative inverse does not exist.
  Modular divide(const Modular &rhs) const { return multiply(rhs.inverse()); }

  // Multiplicative inverse in the modular ring.
  Modular inverse() const { return inverse_mod(value_, modulus); }

  // Compares for equality.
  bool equal(const Modular &rhs) const { return value_ == rhs.value_; }
  bool not_equal(const Modular &rhs) const { return !equal(rhs); }

 protected:
  // An internal value, representing an element in the modular ring.
  // It should always be in the range [0, modulus).
  type value_;

  static constexpr size_t type_width = std::numeric_limits<type>::digits;
  static constexpr size_t modulus_width =
      std::bit_width(static_cast<std::make_unsigned_t<type>>(modulus));

  // Emits a compilation error if addition may overflow.
  void check_addition_overflow() const {
    static_assert(
        modulus_width + 1 <= type_width,
        "Modular addition may overflow. Please use larger integer types.");
  }

  // Emits a compilation error if multiplication may overflow.
  void check_multiplication_overflow() const {
    static_assert(modulus_width * 2 <= type_width,
                  "Modular multiplication may overflow. "
                  "Please use larger integer types.");
  }
};

namespace modular_internal {

// Tests if the type T is the Modular class.
template <typename T>
struct IsModularImpl {
  template <auto mod,
            std::enable_if_t<std::is_same_v<T, Modular<mod>>, bool> = true>
  static constexpr std::true_type test(Modular<mod>);

  static constexpr std::false_type test(...);

  static constexpr bool value = decltype(test(std::declval<T>()))::value;
};

}  // namespace modular_internal

template <typename T>
constexpr bool is_modular = modular_internal::IsModularImpl<T>::value;

template <typename T>
concept ModularType = is_modular<T>;

// Overloads an arithmetic operator with a class method.
#define OVERLOAD_MODULAR_ARITHMETIC_OPERATOR(OP, METHOD) \
  template <ModularType M>                               \
  M operator OP(const M &lhs, const M &rhs) {            \
    return lhs.METHOD(rhs);                              \
  }                                                      \
  template <ModularType M, std::convertible_to<M> T>     \
  M operator OP(const M &lhs, const T &rhs) {            \
    return lhs.METHOD(static_cast<M>(rhs));              \
  }                                                      \
  template <ModularType M, std::convertible_to<M> T>     \
  M operator OP(const T &lhs, const M &rhs) {            \
    return static_cast<M>(lhs).METHOD(rhs);              \
  }

// Overloads an inplace operator with a class method.
#define OVERLOAD_MODULAR_INPLACE_OPERATOR(OP, METHOD) \
  template <ModularType M, std::convertible_to<M> T>  \
  M &operator OP(M &lhs, const T &rhs) {              \
    lhs = lhs.METHOD(static_cast<M>(rhs));            \
    return lhs;                                       \
  }

// Overloads a pair of comparison operator with a class method.
#define OVERLOAD_MODULAR_COMPARISON_OPERATOR(OP, METHOD) \
  template <ModularType M>                               \
  bool operator OP(const M &lhs, const M &rhs) {         \
    return lhs.METHOD(rhs);                              \
  }                                                      \
  template <ModularType M, std::convertible_to<M> T>     \
  bool operator OP(const M &lhs, const T &rhs) {         \
    return lhs.METHOD(static_cast<M>(rhs));              \
  }                                                      \
  template <ModularType M, std::convertible_to<M> T>     \
  bool operator OP(const T &lhs, const M &rhs) {         \
    return static_cast<M>(lhs).METHOD(rhs);              \
  }

// Overloads an unary operator with a class method.
#define OVERLOAD_MODULAR_UNARY_OPERRATOR(OP, METHOD) \
  template <ModularType M>                           \
  M operator OP(const M &x) {                        \
    return x.METHOD();                               \
  }

// Overloads an increment/decrement operator with a class method.
#define OVERLOAD_MODULAR_INCDEC_OPERRATOR(OP, METHOD) \
  /* prefix */                                        \
  template <ModularType M>                            \
  M &operator OP(M &x) {                              \
    x = x.METHOD(M(1));                               \
    return x;                                         \
  }                                                   \
  /* postfix */                                       \
  template <ModularType M>                            \
  M operator OP(M &x, int) {                          \
    M old = x;                                        \
    x = x.METHOD(M(1));                               \
    return old;                                       \
  }

OVERLOAD_MODULAR_ARITHMETIC_OPERATOR(+, add)
OVERLOAD_MODULAR_ARITHMETIC_OPERATOR(-, subtract)
OVERLOAD_MODULAR_ARITHMETIC_OPERATOR(*, multiply)
OVERLOAD_MODULAR_ARITHMETIC_OPERATOR(/, divide)

OVERLOAD_MODULAR_INPLACE_OPERATOR(+=, add)
OVERLOAD_MODULAR_INPLACE_OPERATOR(-=, subtract)
OVERLOAD_MODULAR_INPLACE_OPERATOR(*=, multiply)
OVERLOAD_MODULAR_INPLACE_OPERATOR(/=, divide)

OVERLOAD_MODULAR_COMPARISON_OPERATOR(==, equal)
OVERLOAD_MODULAR_COMPARISON_OPERATOR(!=, not_equal)

OVERLOAD_MODULAR_UNARY_OPERRATOR(-, negate)
OVERLOAD_MODULAR_UNARY_OPERRATOR(+, get)

OVERLOAD_MODULAR_INCDEC_OPERRATOR(++, add)
OVERLOAD_MODULAR_INCDEC_OPERRATOR(--, subtract)

#undef OVERLOAD_ARITHMETIC_OPERATOR
#undef OVERLOAD_INPLACE_OPERATOR
#undef OVERLOAD_COMPARISON_OPERATOR
#undef OVERLOAD_UNARY_OPERRATOR
#undef OVERLOAD_INCDEC_OPERRATOR

// Overloads input stream operator.
template <ModularType M>
std::istream &operator>>(std::istream &stream, M &modular) {
  typename M::type value;
  stream >> value;
  modular.set(std::move(value));
  return stream;
}

// Overloads output stream operator.
template <ModularType M>
std::ostream &operator<<(std::ostream &stream, const M &modular) {
  stream << modular.get();
  return stream;
}

}  // namespace number_theory

using number_theory::inverse_mod;
using number_theory::Modular;

}  // namespace tql

#endif  // NUMBER_THEORY_MODULAR_H_
