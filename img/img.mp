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
end;