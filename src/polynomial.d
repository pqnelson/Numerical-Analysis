import std.stdio, std.conv, std.math;

struct Monomial {
  real coef;
  long exp;
}

class Polynomial {
  real coefs_[];
  size_t degree_;
  @property real[] coefs() { return this.coefs_; }
  @property size_t degree() { return this.degree_; }

  this(real[] newCoefs) {
    this.coefs_ = newCoefs.dup;
    this.degree_ = newCoefs.length-1;
  }

  real eval(real x) {
    real result = 0.0;
    foreach_reverse(real c; this.coefs_)
      result = result*x + c;
    return result;
  }
  Polynomial add(Polynomial p) {
    size_t i,M,N;
    M = this.degree+1;
    N = p.degree+1;
    real coefs[] = new real[N];
    
    for(i = 0; (M>N ? N : M)>i; i++) {
      coefs[i] = p.coefs[i]+this.coefs[i];
    }
    if(M>N) {
      for(i=N; M>i; i++)
        coefs[i] = this.coefs[i];
    } else if(N>M) {
      for(i=M; N>i; i++)
        coefs[i] = p.coefs[i];
    }
    return new Polynomial(coefs);
  }
  Polynomial multiply(Polynomial p) {
    size_t m,M,n,N;
    real pCoefs[] = p.coefs;
    N = pCoefs.length;
    M = coefs.length;
    real coefs[] = new real[M+N-1];

    for(size_t k=0; coefs.length>k; k++)
      coefs[k]=0.0;
    

    for(m=0; M>m; m++)
      for(n=0; N>n; n++)
        coefs[m+n] += pCoefs[m]*(this.coefs_[n]);

    return new Polynomial(coefs);
  }
}

unittest {
  Polynomial p = new Polynomial([0.0,1.0]);
  Polynomial r = p.multiply(p);
  assert(r.eval(2.0)==4.0);
  writefln("We have it be",r.eval(2.0));
}

class DividedDifferences {
  real coefs_[][];
  real x_[];

  this(real[] x, real[] y) {
    if (x.length != y.length)
      throw new Exception("invalid inputs");
    
    this.x_ = x;
    real dx, dy;
    size_t i, j, N;
    N = x.length;
    this.coefs_.length = N;
    
    // initialize the coefficient array
    for(i=0; i<N; i++) {
      this.coefs_[i].length = N;
      this.coefs_[i][0] = y[i];
    }
    
    // compute Newton's divided differences
    for(i=1; i<N; i++) {
      for(j=0; j<i; j++) {
        dy = this.coefs_[i+1][j] - this.coefs_[i][j];
        dx = x[i+1]-x[i+1-j];
        this.coefs_[i][j] = dy/dx;
      }
    }
  }
  
  Polynomial toPolynomial() {
    size_t i;
    size_t N = this.coefs_.length;
    real C[] = new real[N];

    // get the main coefficients
    for(i=0; N>i; i++)
      C[i]=this.coefs_[i][i];

    // create a polynomial
    Polynomial result = new Polynomial([C[N-1]]);
    for(i=N-1; i>0; i--) {
      result = result.multiply(new Polynomial([-this.x_[i],1.0]));
      result = result.add(new Polynomial([C[i-1]]));
    }
    return result;
  }
}

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

int main() {
  real c[2];
  c[0] = 0.0;
  c[1] = 1.0;
  Polynomial p = new Polynomial(c);
  Polynomial r = p.add(p);
  writefln("We have "~to!string(r.eval(2.0)));
  return 0;
}
