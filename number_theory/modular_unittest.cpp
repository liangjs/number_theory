#include <stdint.h>

#include <limits>
#include <type_traits>

#include <gtest/gtest.h>

#include "number_theory/modular.h"

namespace tql {
namespace number_theory {

template <typename T>
void test_modular_basic() {
  using Mod10 = Modular<T(10)>;
  static_assert(std::is_same<typename Mod10::type, T>::value);
  static_assert(Mod10::modulus == T(10));

  // test constructors
  Mod10 a = T(123);
  Mod10 b = a;
  Mod10 c;
  EXPECT_EQ(a.get(), T(3));
  EXPECT_EQ(b.get(), T(3));
  EXPECT_EQ(c.get(), T(0));

  // test assignments
  if (std::numeric_limits<T>::is_signed) {
    c = T(-1);
    a.set(T(-4));
  } else {
    c = T(9);
    a.set(T(6));
  }
  EXPECT_EQ(c.get(), T(9));
  EXPECT_EQ(a.get(), T(6));
  a = std::move(c);
  EXPECT_EQ(a.get(), T(9));

  // test conversions
  a = T(3);
  EXPECT_EQ(static_cast<T>(a), T(3));
  EXPECT_EQ(static_cast<Mod10>(T(15)).get(), T(5));
  // implicit conversions
  T x = a;
  EXPECT_EQ(x, T(3));
  EXPECT_EQ(Mod10{15}.get(), 5);

  // test arithmetics
  a = T(6), b = T(4);
  c = a.add(b);
  EXPECT_EQ(c.get(), T(0));
  c = b.negate();
  EXPECT_EQ(c.get(), T(6));
  c = b.subtract(a);
  EXPECT_EQ(c.get(), T(8));
  c = a.multiply(b);
  EXPECT_EQ(c.get(), T(4));

  // test comparisons
  c = T(3);
  b = c;
  a = T(0);
  EXPECT_TRUE(b.equal(c));
  EXPECT_FALSE(a.equal(b));
}

TEST(ModularTest, Basic) {
  test_modular_basic<int16_t>();
  test_modular_basic<int32_t>();
  test_modular_basic<int64_t>();
  test_modular_basic<uint16_t>();
  test_modular_basic<uint32_t>();
  test_modular_basic<uint64_t>();
}

template <typename T>
void test_modular_operators() {
  using Mod10 = Modular<T(10)>;

  // test addition
  Mod10 a = 7, b = 5;
  EXPECT_EQ(a + b, 2);
  EXPECT_EQ(a + 3, 0);
  EXPECT_EQ(3 + a, 0);
  b += a;
  EXPECT_EQ(b, 2);
  a += 4;
  EXPECT_EQ(a, 1);

  // test subtraction
  a = 7, b = 5;
  EXPECT_EQ(b - a, 8);
  EXPECT_EQ(a - 8, 9);
  EXPECT_EQ(0 - a, 3);
  b -= a;
  EXPECT_EQ(b, 8);
  a -= 10;
  EXPECT_EQ(a, 7);

  // test multiplication
  a = 7, b = 5;
  EXPECT_EQ(a * b, 5);
  EXPECT_EQ(a * 2, 4);
  EXPECT_EQ(3 * a, 1);
  b *= a;
  EXPECT_EQ(b, 5);
  a *= 10;
  EXPECT_EQ(a, 0);

  // test comparison
  EXPECT_EQ(Mod10(1), 11);
  EXPECT_EQ(11, Mod10(1));
  EXPECT_EQ(Mod10(1), Mod10(11));
  EXPECT_NE(1, Mod10(3));
  EXPECT_NE(Mod10(3), 1);
  EXPECT_NE(Mod10(1), Mod10(3));

  // test unary operators
  a = 7;
  EXPECT_EQ(+a, 7);
  EXPECT_EQ(-a, 3);

  // test increment/decrement
  a = 9;
  EXPECT_EQ(a++, 9);
  EXPECT_EQ(a, 0);
  EXPECT_EQ(++a, 1);
  EXPECT_EQ(a, 1);
  EXPECT_EQ(a--, 1);
  EXPECT_EQ(a, 0);
  EXPECT_EQ(--a, 9);
  EXPECT_EQ(a, 9);
}

TEST(ModularTest, Operators) {
  test_modular_operators<int16_t>();
  test_modular_operators<int32_t>();
  test_modular_operators<int64_t>();
  test_modular_operators<uint16_t>();
  test_modular_operators<uint32_t>();
  test_modular_operators<uint64_t>();
}

}  // namespace number_theory
}  // namespace tql
