outputtemplate := "%j-%c.mps";
beginfig(0)
  numeric u; u:= 1pc;
  numeric i;

  for i=1 upto 9:
    draw (i*0.1u, -2)--(i*0.1u, 2);
    if i>1:
      draw (i*u, -4)--(i*u, 4);
    fi;
  endfor;
  % draw (10u, -8)--(10u, 8);
  % draw (20u, -8)--(20u, 8);
  draw (-u, 0)--(22u, 0);
  for i=0 upto 2:
    draw (i*10u, -.5u)--(i*10u, .5u) withcolor red;
  endfor;
  draw (u,-4)--(u,4) withcolor red;
  label(btex $0$ etex, (0, -pc));
  label(btex $1$ etex, (1u, -pc));
  label(btex $10$ etex, (10u, -pc));
  label(btex $20$ etex, (20u, -pc));
endfig;

beginfig(1)
  numeric u; u:= 1pc;
  numeric v; v:=0.5u;
  numeric i;
  path p[];
  z[0] = (0,5u); % x^{*}
  z[1] = (0,0);   % x
  z[2] = (8u,u);  % y
  z[3] = (8u,6u); % y^{*}

  label.lft(btex $x^{*}$ etex, z0);
  label.lft(btex $x$ etex, z1);
  label.rt(btex $y$ etex, z2);
  label.rt(btex $y^{*}$ etex, z3);

  for i=0 upto 3:
    label(btex $\bullet$ etex, z[i]);
  endfor;

  p[0] := z1--(z3+(-3,-3)); % x -> y*
  p[1] := z1--(z0 + (0,-3)); % x -> x*
  p[2] := z0--((z0--z3) intersectionpoint ((fullcircle scaled v) shifted z3)); % x* -> y*
  p[3] := z1--((z1--z2) intersectionpoint ((fullcircle scaled v) shifted z2)); % x -> y
  p[4] := z2--((z2--z3) intersectionpoint ((fullcircle scaled v) shifted z3)); % y -> y*

  drawarrow p0 dashed evenly;
  for i=1 upto 4:
    drawarrow p[i];
  endfor;
  label.top(btex $f$ etex, .5[z0,z3]);
  label.bot(btex $f$ etex, .5[z1,z2]);
  label.ulft(btex alg etex, .5[z1,z3]);
endfig;
end;