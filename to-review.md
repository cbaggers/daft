# To Review

## Ugly hacks
#'in-screen-p
e27a00a * ugly ass pile of path hacks for shipping
e105ad9 * haha jesus, it was calling resize every frame
e28935f * hack in resize listener
6c2bbe4 * system path hack

## Code Quality Cleanup
8fb61ac * anim hack, this need cleanup
eb56f23 * default gamepad

## Things to fix
- we clearly need global rotation for cases like bomber-chap
  dude walking different direcions without rotating the image
- names of animation functions are super vague
- depth getter/setter are absolute, should the be relative?
  (Depths are from 0 to 100 with 100 being the furthest away)
- timers currently have global names. How do we want to handle
  resources like this given that our little engine doesnt really
  have much to say about instances of stuff being passed around.
- scale is absolute, is this ok?
- position-between & offset-to were neccessary for bomberchap
  but don't fit our model

## Things to add
- a way to reset (see orb for a case where we wanted this)
- text system (with custom sheets) .. maybe msdf?
-

## Seems fine (idea wise at least)
e9e37ff * zero out c-arrays before use
9ae4014 * dont use %pos & %rot where possible
0670318 * move some code around and get frame modifier `except not fixed timestep`
9b6b6d3 * add shipshape
81133c7 * resize on start
dd70936 * Hacked in order independent transparency
3983b87 * using a quad rather than a cube now
168d121 * actors-kinds get their own instance streams `static actors`
a285ce0 * basic sound support `execution probably terrible but idea fine`
7348a20 * seperate collision masks & origin taken into account
3042af1 * add camera focus offset
309140d * add focus-camera
3c35a01 * export god an spawn
93a11a9 * clamp to edge
6a93ac0 * anim length lives on the kind now

93d5441 * add visual origin to actors `check behavior, might be fine`
06eaca6 * snapping position `check this is local`
c2f6b68 * per-second
