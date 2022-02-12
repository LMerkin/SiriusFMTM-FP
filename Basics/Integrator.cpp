#include <type_traits>
#include <cmath>
#include <stdexcept>
#include <functional>
#include <cassert>
#include <iostream>
#include <memory>
#include <sys/time.h>

namespace
{
  enum class D: int
  {
    UNDEFINED = 0,
    X     = 1,
    Const = 2,
    Add   = 3,
    Sub   = 4,
    Mult  = 5,
    Div   = 6
  };

  template<typename A>
  struct AST1
  {
    static_assert(std::is_floating_point_v<A>);

    D                     m_D;
    A                     m_A;
    std::shared_ptr<AST1> m_left;
    std::shared_ptr<AST1> m_right;

    // Default Ctor:
    AST1()
    : m_D    (D::UNDEFINED),
      m_A    (NAN),
      m_left (),
      m_right()
    {}

    // Non-Default Ctor:
    AST1
    (
      D a_D,
      A a_A  = A(NAN),
      std::shared_ptr<AST1> const& a_left  = std::shared_ptr<AST1>(),
      std::shared_ptr<AST1> const& a_right = std::shared_ptr<AST1>()
    )
    : m_D    (a_D),
      m_A    (a_A),
      m_left (a_left),
      m_right(a_right)
    {
      // Checks:
      assert( m_D != D::UNDEFINED);
      assert((m_D == D::Const) == !std::isnan(m_A));
      assert((m_D != D::X  && m_D != D::Const) ==
             (bool(m_left) && bool(m_right)));
    }

    // Dtor:
    ~AST1() = default;

    // "MkFunc":
    std::function<A(A)> MkFunc() const
    {
      switch (m_D)
      {
        case D::X:
          return std::function<A(A)>([](A a_x)->A   { return a_x; });

        case D::Const:
        {
          A c = this->m_A;
          return std::function<A(A)>([c](A _)->A { return c; });
        }

        case D::Add:
        {
          std::function<A(A)> leftF  = this->m_left  ->MkFunc();
          std::function<A(A)> rightF = this->m_right ->MkFunc();
          return std::function<A(A)>
                 ([leftF, rightF](A a_x)->A
                  { return leftF(a_x) + rightF(a_x); });
        }

        case D::Sub:
        {
          std::function<A(A)> leftF  = this->m_left  ->MkFunc();
          std::function<A(A)> rightF = this->m_right ->MkFunc();
          return std::function<A(A)>
                 ([leftF, rightF](A a_x)->A
                  { return leftF(a_x) - rightF(a_x); });
        }

        case D::Mult:
        {
          std::function<A(A)> leftF  = this->m_left  ->MkFunc();
          std::function<A(A)> rightF = this->m_right ->MkFunc();
          return std::function<A(A)>
                 ([leftF, rightF](A a_x)->A
                  { return leftF(a_x) * rightF(a_x); });
        }

        case D::Div:
        {
          std::function<A(A)> leftF  = this->m_left  ->MkFunc();
          std::function<A(A)> rightF = this->m_right ->MkFunc();
          return std::function<A(A)>
                 ([leftF, rightF](A a_x)->A
                  { return leftF(a_x) / rightF(a_x); });
        }

        default:
          throw std::invalid_argument("Invalid D");
      }
    }
  };

  // Generic Integration Function:
  template<typename A, typename Func>
  A Integrate(Func const& a_f, A const& a_a, A const& a_b, long a_n_steps)
  {
    // Rectangles  method:
    assert(a_a <= a_b && a_n_steps > 0);
    A step = (a_b - a_a) / A(a_n_steps);
    A x    = a_a;
    A s    = A(0);
    while (x < a_b)
    {
      s += a_f(x);
      x += step;
    }
    s *= step;
    return s;
  }

  // Raw Integrand:
  inline double FR(double a_x) { return a_x + 1.0; }

  // With Overloaded ():
  struct FO
    { double operator() (double a_x) const { return a_x + 1.0; } };
}

int main()
{
  using ASTD = AST1<double>;

  // Substitute for Haskell "read" (String -> AST1):
  // (X+1):
  std::shared_ptr<ASTD> X = std::make_shared<ASTD>(D::X);

  std::shared_ptr<ASTD> Const1 =
    std::make_shared<ASTD>(D::Const, 1.0);

  std::shared_ptr<ASTD> Top     =
    std::make_shared<ASTD>(D::Add, double(NAN), X, Const1);

  // Convert it to a function:
  std::function<double(double)> func = Top->MkFunc();

  // Integrate it from 0 to 1 by rectangles:
  long N = 100'000'000;
  // Get the integration time:
  timeval t0;
  gettimeofday(&t0, nullptr);
  double s = Integrate(func, 0.0, 1.0, N);
  timeval t1;
  gettimeofday(&t1, nullptr);

  long usec = (t1.tv_sec - t0.tv_sec) * 1'000'000 + (t1.tv_usec - t0.tv_usec);
  std::cout << "std::func:    "
            << s << " in " << double(usec)*1e-6 << " sec" << std::endl;

  // Same by using an inlined lambda:
  auto FL = [](double x) -> double { return x + 1.0; };

  gettimeofday(&t0, nullptr);
  s = Integrate(FL, 0.0, 1.0, N);
  gettimeofday(&t1, nullptr);

  usec = (t1.tv_sec - t0.tv_sec) * 1'000'000 + (t1.tv_usec - t0.tv_usec);
  std::cout << "Lambda:       "
            << s << " in " << double(usec)*1e-6 << " sec" << std::endl;

  // Same by using a raw (plain old) function:
  gettimeofday(&t0, nullptr);
  s = Integrate(FR, 0.0, 1.0, N);
  gettimeofday(&t1, nullptr);

  usec = (t1.tv_sec - t0.tv_sec) * 1'000'000 + (t1.tv_usec - t0.tv_usec);
  std::cout << "Plain Old:    "
            << s << " in " << double(usec)*1e-6 << " sec" << std::endl;

  // Same by using () overloading:
  gettimeofday(&t0, nullptr);
  s = Integrate(FO(), 0.0, 1.0, N);
  gettimeofday(&t1, nullptr);

  usec = (t1.tv_sec - t0.tv_sec) * 1'000'000 + (t1.tv_usec - t0.tv_usec);
  std::cout << "OverLoaded(): "
            << s << " in " << double(usec)*1e-6 << " sec" << std::endl;
}
