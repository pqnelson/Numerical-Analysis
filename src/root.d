import std.math;

real bisection(real f(real), real _a, real _b, real tol=1e-14, int NMAX = 60)
{
  real a = _a;
  real b = _b;
  real c = 0.0;
  for(int i=0; i < NMAX; i++) {
    c = 0.5*(a+b);
    if (f(c) == 0 || 0.5*math.abs(b-a)<tol)
      return c;
    if (math.sgn(f(c)) == math.sgn(f(a))) 
      a = c;
    else
      b = c;
  }
  return c;
}

real newton(real f(real), real df(real), real _a, real tol=1e-14, int NMAX = 9)
{
  real a = _a;
  real b, y, dy;
  for(int i=0; i<NMAX; i++) {
    y = f(a);
    dy = df(a);

    if(math.abs(dy)<tol) {
      throw Exception("Warning: denominator too close to zero");
    }

    b = a - (y/dy);

    if (math.abs(b-a)/math.abs(b) < tol)
      return b;

    a = b;
  }
  return b;
}

real secant(real f(real), real _a, real _b, real tol=1e-14, int NMAX=12) 
{
  real a, b, y, z, tmp;
  a = _a;
  b = _b;
  y = f(a);
  z = f(b);
  for(int i=0; i<NMAX; i++) {
    
    if(math.abs(z-y)<tol) {
      throw Exception("Warning: denominator too close to zero");
    }
    tmp = (a*z - b*y)/(z-y);
    a = b;
    b = tmp;
    y = z;
    z = f(b);
  }
  return b;
}
