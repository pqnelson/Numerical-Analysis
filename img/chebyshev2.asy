import graph;
size(0,150);

real a=-1, b=1;

real f6(real x) {
   real xx = x*x;
   return 0.99490950226244343891 + xx*(-0.84841628959276018100 + xx*0.35350678733031674208);
}

real h6(real x) {
     real xx = x*x;
     return 0.56730769230769230769 + xx*(-1.7307692307692307692 + xx*1.2019230769230769231);
}

real h10(real x) {
     real xx = x*x;
     return 0.86153815196581887869 + xx*(-8.2609233283477396090 + xx*(
     30.728530042096301525 + xx*(-44.915458080892001307 + 21.624774753639158975*xx)));
}

real g(real x) {return 1/(1 + 25*(x^2));}

draw(graph(h10,a,b,operator ..),red);
draw(graph(g,a,b,operator ..),blue);

xaxis(LeftTicks(NoZero));
yaxis(LeftTicks(NoZero));


int n=5;

a = -1;
b = 1;
real width=(b-a)/((real) n);
for(int i=0; i <= n; ++i) {
  real x=a+width*i;
  draw((x,g(x))--(x,h6(x)));
}
