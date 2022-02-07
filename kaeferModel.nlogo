;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________
;------------ EXTENSIONS and VARIABLES ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________

;extensions [profiler vid]
globals
[
  ;  max_ticks            ; integer, maximum number of ticks until simulation is stopped
  ;  n_beetles            ; integer, number of initialized beetles
  ;  worldsize            ; integer, indicates the worlds length and width
  ;  seed                 ; integer, that is set as seed for random functions

  ;  turning_angle_sd     ; float, standard deviation of turning angle for beetle movement
;    step_size_mean       ; float, mean of gamma distribution of step size for beetle movement
;    step_size_variance   ; float, variance of gamma distribution of step size for beetle movement
    step_size_alpha      ; float, alpha value for gamma distribution of step size for beetle movement, calculated from mean and variance
    step_size_lambda     ; float, lambda value for gamma distribution of step size for beetle movement, calculated from mean and variance
  ;  bias                 ; float in [0,1], weights influence of attractor on turning angle

  ;  include_traps?       ; bool, indicates if traps should be included
  ;  init_beetles         ; string, initial beetle distribution is either "normal" or "random", chosen in view
  ;  init_sd              ; float, standard deviation of initial beetle position

  kettle_hole_patches   ; patchset, containing all kettle hole patches
  border_patches        ; patchset, containing all patches at the border of the landscape

  ;  trap_scenario      ; string, given by chooser in view
  wall_closeness        ; float, how close should beetles be set to wall
]
patches-own
[
  is_kettle_hole?              ; boolean, indicates if patch is part of a kettle hole, if false patch is a field patch
]
breed [beetles beetle]
breed[traps trap]
breed[pitfall_traps pitfall_trap]
traps-own
[
  catching_radius       ; float, distance to trap position in that beetles will be catched
  wall_radius           ; float, radius of walls from trap position
  rotation              ; float, rotation of trap in degree
]
pitfall_traps-own
[
  my_quadrant           ; integer, showing quadrant of this pitfall-trap within cross
  n_trapped_beetles     ; integer, counts beetles that are trapped here
]

beetles-own
[
  closest_trap          ; trap-agent, that has the minimum distance to beetle
  trap_quadrant         ; integer, beetle is in this quadrant of closest trap
  closest_wall          ; string, one of "left"/"right", indicating if closest wall is to the left or right when facing the center of the trap
  start_xcor            ; float, saves x coordinate when setup
  start_ycor            ; float, saves y coordinate when setup
]

;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________
;
;------------ SETUP PROCEDURES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________

to setup
  ca
  reset-ticks

  if seed != 0
  [
    random-seed seed
  ]

  resize-world (- worldsize / 2) (worldsize / 2) (- worldsize / 2) (worldsize / 2)
  set-patch-size 5 * 100 / worldsize

  if include_traps?
  [
    set wall_closeness 0.99
    setup_traps
  ]
  if bias != 0
  [
    show_landscape
  ]
  ; setup beetles
  create-beetles n_beetles [ setup_beetles ]
  set step_size_alpha (step_size_mean * step_size_mean / step_size_variance)
  set step_size_lambda (step_size_mean / step_size_variance)
end

to setup_traps ; setup landscape with traps based on chooser input
  let trap_list 0
  ifelse trap_scenario = "original"
  [
    set trap_list [[0 6.5 0.0785 1 45] [2 13.5 0.0785 1 45] [0 -6.5 0.0785 1 45] [-2 -13.5 0.0785 1 45] [6.5 0 0.0785 1 45] [13.5 -2 0.0785 1 45] [-6.5 0 0.0785 1 45] [-13.5 2 0.0785 1 45]]
  ]
  [
    ifelse trap_scenario = "one_circle"
    [
      set trap_list [[0 6.5 0.0785 1 45] [0 -6.5 0.0785 1 45]  [6.5 0 0.0785 1 45] [-6.5 0 0.0785 1 45]]
    ]
    [
      ifelse trap_scenario = "three_circles"
      [
        set trap_list [[0 6.5 0.0785 1 45] [2 13.5 0.0785 1 45] [4 20.5 0.0785 1 45] [0 -6.5 0.0785 1 45] [-2 -13.5 0.0785 1 45] [-4 -20.5 0.0785 1 45] [6.5 0 0.0785 1 45] [13.5 -2 0.0785 1 45] [20.5 -4 0.0785 1 45] [-6.5 0 0.0785 1 45] [-13.5 2 0.0785 1 45] [-20.5 4 0.0785 1 45]]
      ]
      [
        ifelse trap_scenario = "higher_distance"
        [
          set trap_list [[0 6.5 0.0785 1 45] [2 20.5 0.0785 1 45] [0 -6.5 0.0785 1 45] [-2 -20.5 0.0785 1 45] [6.5 0 0.0785 1 45] [20.5 -2 0.0785 1 45] [-6.5 0 0.0785 1 45] [-20.5 2 0.0785 1 45]]
        ]
        [
          ifelse trap_scenario = "same_circular_arc"
          [
            set trap_list [[0 6.5 0.0785 1 45] [3 13.5 0.0785 2.166174 45] [0 -6.5 0.0785 1 45] [-3 -13.5 0.0785 2.166174 45] [6.5 0 0.0785 1 45] [13.5 -3 0.0785 2.166174 45] [-6.5 0 0.0785 1 45] [-13.5 3 0.0785 2.166174 45]]
          ]
          [
            ifelse trap_scenario = "optimal_offset"
            [
              set trap_list [[0 6.5 0.0785 1 45] [9.7 9.7 0.0785 1 0] [0 -6.5 0.0785 1 45] [9.7 -9.7 0.0785 1 0][6.5 0 0.0785 1 45] [-9.7 -9.7 0.0785 1 0][-6.5 0 0.0785 1 45][-9.7 9.7 0.0785 1 0]]
            ]
            [
              error (word "trap_scenario is none of the suggested, but is: " trap_scenario)
            ]
  ]]]]]
  foreach trap_list [ setting ->
    create-traps 1
    [
      setxy item 0 setting item 1 setting
      set catching_radius item 2 setting
      set wall_radius item 3 setting
      set rotation item 4 setting
      visualize_trap
      setup_pitfall_traps
    ]
  ]
end

to visualize_trap     ; trap procedure
                      ; draws a cross and catching circle
                      ; draw cross
  let x xcor
  let y ycor
  set color orange
  pen-down
  set heading rotation
  repeat 4
  [
    jump wall_radius
    setxy x y
    set heading heading + 90
  ]
  ; draw trap
  set shape "circle"
  set color [255 0 0 200]
  set size (2 * catching_radius)
end

to setup_pitfall_traps  ; trap procedure
                        ; hatches the pitfall traps for each quadrant
  let temp_n 0
  repeat 4 [
    hatch-pitfall_traps 1
    [
      set temp_n temp_n + 1
      set my_quadrant temp_n
      set n_trapped_beetles 0
      create-link-with myself
    ]
  ]
end

to setup_beetles  ; beetle procedure
  ifelse init_beetles = "random" [
    setxy random-xcor random-ycor
  ]
  [
    ifelse init_beetles = "normal" [
      setxy random-normal 0 init_sd random-normal 0 init_sd
    ]
    [
      ifelse init_beetles = "equal" [
        ; do nothing, as xy coordinates are set by patch
      ]
      [error (word "init_beetles should be one of normal/random/equal, but is: " init_beetles)
      ]
    ]
  ]
  set start_xcor xcor
  set start_ycor ycor
  set heading random 360
  if include_traps?
  [
    update_trap
  ]
  set color red
  ;  color_beetles
;  pen-down
end

; From File Input Example - small modifications
; This procedure lets the user choose the file to load from.
to show_landscape
  ask patches
  [
    set is_kettle_hole? distance patch 0 0 < 6
    if is_kettle_hole? [set pcolor green]
  ]
  set kettle_hole_patches patches with [is_kettle_hole?]
end

;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________
;
;------------ GO PROCEDURES -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________

to go
  tick
  if (ticks > max_ticks or not any? beetles)
  [
    stop
  ]

  ask beetles
  [
    ;    color_beetles
    move
    if include_traps?
    [
      update_trap
      if distance closest_trap < [catching_radius] of closest_trap
      [
        ; get_trapped
        ask closest_trap [update_counters]
        die
      ]
    ]
  ]
end

to color_beetles
  ;    set color (12 + floor ( ticks / (max_ticks) * 7)) ; for brigther color in later ticks
  if color != 17
  [
    ; if beetle has left inner 90% of world
    ifelse pxcor = max-pxcor or pxcor = min-pxcor or pycor = max-pycor or pycor = min-pycor
    [
      set color 17
    ]
    [
      set color 13
    ]
  ]
end

to move  ; beetle procedure
         ; beetles move with a biased correlated random walk
         ; if they cross a wall, they adjust their heading
  let x_old xcor
  let y_old ycor

  ; if beetle should move biased
  ; else it should move correlated
  ifelse (kettle_hole_patches != 0 and abs(bias) > random-float 1)
  [
    ; if beetle should move towards from kettle hole
    ; else it should move away from kettle hole
    ifelse bias < 0
    [
      face patch 0 0
      set heading (heading + 180)
    ]
    [
      face min-one-of kettle_hole_patches [distance myself]
    ]
  ]
  [
    ; beetle should move in a CRW
    set heading random-normal heading turning_angle_sd
  ]

  let step_size random-gamma step_size_alpha step_size_lambda
  jump step_size

  if include_traps?
  [
    if closest_trap = min-one-of traps [distance myself] and distance closest_trap < 10; beetles closest trap is the same
    [
      let new_quadrant first get_quadrant
      if trap_quadrant != new_quadrant  ; beetle has changed quadrant
      [
        let x_trap [xcor] of closest_trap
        let y_trap [ycor] of closest_trap
        let old_distance_to_closest_trap sqrt ( (x_old - x_trap) * (x_old - x_trap) + (y_old - y_trap) * (y_old - y_trap) )
        ; if beetle has moved across wall
        if distance closest_trap < [wall_radius] of closest_trap or old_distance_to_closest_trap < [wall_radius] of closest_trap
        [
;          pen-erase
          setxy x_old y_old
;          pen-down

          ; calculate intersection of movement path with wall
          let wall_heading get_wall_heading trap_quadrant new_quadrant
          let s (get_intersection wall_heading)
          let delta_x s * sin heading
          let delta_y s * cos heading

          ; if intersection is too far away forward normally and stop this procedure
          if sqrt (delta_x * delta_x + delta_y * delta_y) > step_size
          [
            jump step_size
            stop
          ]

          ; move until close to wall
          setxy (xcor + wall_closeness * delta_x) (ycor + wall_closeness * delta_y)
          update_trap
          ; adjust heading
          ifelse (heading - wall_heading) mod 360 > 90 and (heading - wall_heading) mod 360 < 270
          [
            set heading wall_heading + 180
          ]
          [
            set heading wall_heading
          ]
        ]
      ]
    ]
  ]
end

to update_trap     ; beetle procedure
                   ; updates trap information w.r.t. current position
  set closest_trap min-one-of traps [distance myself]
  if distance closest_trap < 10
  [
    let result get_quadrant
    set trap_quadrant first result
    set closest_wall last result
  ]
end

to update_counters     ; trap procedure
                       ; myself is catched beetle
                       ; increases counter of quadrant that the catched beetle has been in
  let quadrant [trap_quadrant] of myself
  let this_pitfall_trap link-neighbors with [my_quadrant = quadrant]
  ask this_pitfall_trap
  [
    set n_trapped_beetles n_trapped_beetles + 1
  ]
end

;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________
;
;------------ REPORTERS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;_________________________________________________________________________________________________________________________________________________________________________________________________________________________________


to-report get_quadrant  ; beetle procedure
                        ; reports quadrant of closest trap
  let x_center [xcor] of closest_trap
  let y_center [ycor] of closest_trap
  let alpha [rotation] of closest_trap
  let x_dist xcor - x_center
  let y_dist ycor - y_center
  let x_rot ((cos alpha) * (minimal_distance_x x_dist) - (sin alpha) * (minimal_distance_y y_dist))
  let y_rot (((sin alpha) * (minimal_distance_x x_dist)) + (cos alpha) * (minimal_distance_y y_dist))
  let quadrant "error"
  let wall "error"
  ifelse x_rot > 0
  [
    ifelse y_rot > 0
    [
      set quadrant 1
      ifelse x_rot < y_rot
      [set wall "right"]
      [set wall "left"]
    ]
    [
      set quadrant 2
      ifelse (abs x_rot) < (abs y_rot)
      [set wall "left"]
      [set wall "right"]
    ]
  ]
  [
    ifelse y_rot > 0
    [
      set quadrant 4
      ifelse (abs x_rot) < (abs y_rot)
      [set wall "left"]
      [set wall "right"]
    ]
    [
      set quadrant 3
      ifelse x_rot < y_rot
      [set wall "left"]
      [set wall "right"]
    ]
  ]
  report list quadrant wall
end

to-report minimal_distance_x [dist]   ; reports minimal distance, irrespective of sign
                                      ; (e.g. -5 instead of 45 in a world with width 50)
  let dist1 dist
  let dist2 0
  ifelse dist > 0
  [
    set dist2 dist - world-width
  ]
  [
    set dist2 dist + world-width
  ]
  if abs dist1 < abs dist2
  [
    report dist1
  ]
  report dist2
end

to-report minimal_distance_y [dist]  ; reports minimal distance, irrespective of sign
                                     ; (e.g. -5 instead of 45 in a world with height 50)
  let dist1 dist
  let dist2 0
  ifelse dist > 0
  [
    set dist2 dist - world-height
  ]
  [
    set dist2 dist + world-height
  ]
  if abs dist1 < abs dist2
  [
    report dist1
  ]
  report dist2
end

to-report get_wall_heading [quadrant_1 quadrant_2]   ; beetle procedure
                                                     ; reports heading of wall
  if quadrant_2 < quadrant_1 ; quadrants should be sorted
  [
    let temp quadrant_1
    set quadrant_1 quadrant_2
    set quadrant_2 temp
  ]
  if quadrant_2 = 4 and quadrant_1 = 1
  [
    set quadrant_1 4
    set quadrant_2 1
  ]
  let wall_rotation [rotation] of closest_trap + (quadrant_1 mod 2) * 90
  if quadrant_2 - quadrant_1 = 2 and closest_wall = "right"   ; in case the beetle crosses two quadrant
  [
    set wall_rotation [rotation] of closest_trap + ((quadrant_2 - 1) mod 2) * 90
  ]
  report wall_rotation
end

to-report get_intersection [wall_heading]   ; beetle procedure
                                            ; reports factor s for formula
                                            ; formula for x = xcor + s * sin heading
                                            ; formula for y = ycor + s * cos heading
                                            ; correct x and y coordinates of trap for world wrapping
  let x_trap_wrapped [xcor] of closest_trap
  ifelse abs ([xcor] of closest_trap - xcor) > abs ([xcor] of closest_trap + world-width - xcor)
  [
    set x_trap_wrapped [xcor] of closest_trap + world-width
  ]
  [
    if abs([xcor] of closest_trap - xcor) > abs([xcor] of closest_trap - world-width - xcor)
    [
      set x_trap_wrapped [xcor] of closest_trap - world-width
    ]
  ]
  let y_trap_wrapped [ycor] of closest_trap
  ifelse abs ([ycor] of closest_trap - ycor) > abs ([ycor] of closest_trap + world-height - ycor)
  [
    set y_trap_wrapped [ycor] of closest_trap + world-height
  ]
  [
    if abs([ycor] of closest_trap - ycor) > abs([ycor] of closest_trap - world-height - ycor)
    [
      set y_trap_wrapped [ycor] of closest_trap - world-height
    ]
  ]
  ; see math formula in Modelling Notebook 2021-04-22
  if sin wall_heading = 0
  [
    report (x_trap_wrapped - xcor ) / sin heading
  ]
  let cotangent cos wall_heading / sin wall_heading
  let denominator (sin heading * cotangent - cos heading)
  if denominator = 0  ; denominator is 0, if headings are parallel
  [
    error "denominator is 0"
  ]
  let numerator ycor - y_trap_wrapped + (x_trap_wrapped - xcor) * cotangent
  let s_beetle numerator / denominator
  report s_beetle
end

to-report report_pitfall_list ; pitfall procedure
                             ; reports a list of catching trap data

  let summary (list xcor ycor my_quadrant n_trapped_beetles)

  report summary
end


to get_runtime_profile
;  profiler:start         ;; start profiling
;  setup                  ;; set up the model
;  repeat 3000 [ go ]       ;; run something you want to measure
;  profiler:stop          ;; stop profiling
;  print profiler:report  ;; view the results
;  profiler:reset         ;; clear the data
end

to get_video
;  ;; export a 30 frame movie of the view
;  setup
;  load_landscape
;  vid:start-recorder
;  vid:record-view ;; show the initial state
;  repeat max_ticks
;  [ go
;    vid:record-view ]
;  vid:save-recording (word "../outputs/" video_name ".mp4")
end
@#$#@#$#@
GRAPHICS-WINDOW
543
36
1059
553
-1
-1
8.333333333333334
1
10
1
1
1
0
1
1
1
-30
30
-30
30
0
0
1
ticks
30.0

BUTTON
9
17
82
50
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
93
96
156
129
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
16
96
80
129
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
294
374
474
407
turning_angle_sd
turning_angle_sd
5
100
5.0
5
1
NIL
HORIZONTAL

SLIDER
296
328
482
361
step_size_variance
step_size_variance
0.1
5
0.1
0.1
1
NIL
HORIZONTAL

SWITCH
21
338
182
371
include_traps?
include_traps?
0
1
-1000

SLIDER
291
96
463
129
n_beetles
n_beetles
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
289
33
461
66
max_ticks
max_ticks
10
8000
7960.0
50
1
NIL
HORIZONTAL

SLIDER
296
416
468
449
bias
bias
-1
1
0.0
0.01
1
NIL
HORIZONTAL

BUTTON
12
246
167
279
NIL
show_landscape
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
293
142
431
187
init_beetles
init_beetles
"random" "normal" "equal"
1

SLIDER
298
206
470
239
init_sd
init_sd
0
16
15.0
1
1
NIL
HORIZONTAL

SLIDER
296
280
477
313
step_size_mean
step_size_mean
0
5
0.1
0.05
1
NIL
HORIZONTAL

CHOOSER
23
387
202
432
trap_scenario
trap_scenario
"original" "one_circle" "three_circles" "higher_distance" "same_circular_arc" "optimal_offset"
5

SLIDER
15
136
187
169
worldsize
worldsize
10
100
60.0
10
1
NIL
HORIZONTAL

INPUTBOX
95
17
214
77
seed
0.0
1
0
Number

BUTTON
1148
89
1318
122
NIL
get_runtime_profile\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1151
147
1252
180
NIL
get_video\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1157
221
1442
281
video_name
bias-0.05
1
0
String

BUTTON
9
286
170
319
white background
ask patches with [pcolor = black][set pcolor white]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
# todos:
- quellen einfügen und sortieren
- generell hübscher machen

# Model description
This model description follows the ODD (Overview, Design concepts, Details) protocol for describing individual- and agent-based models [Grimm2006], as updated by [Grimm2020]. 
The model was implemented in Netlogo 6.1.1 [Wilensky1999] and is available at https://github.com/Kartoffelkeks/kaefermodell.

## Purpose and patterns
The purpose of this model is to study the movement of ground beetles (Carabidae) in agricultural/wheat fields around kettle holes. Using the virtual ecologist approach, the model should give more insights into a real field experiment conducted by Nadja Heitmann [Heitmann2022]. In this experiment, beetles were caught using pitfall traps separated by cross shaped walls in order to determine the direction which the beetles came from.

Furthermore the model will be used to test whether different initial distribution and walking behaviors result in different "trapping patterns", i.e. the number of beetles per trap, and if different trapping arrangements lead to a clearer distinction of behaviors. Furthermore, the model will be used to reconstruct the movement parameters of beetles in artificial and real experiments.
## Entities, state variables, and scales
The model includes three entities: patches, beetles and traps. Their state variables are described in the following table.
The _patches_ are squares, which form the landscape, in which the beetles move. Their position is defined with two integers. They can be either protected area patches or agricultural matrix patches. The patch size is 1 m x 1 m.
The _beetles_ have a current position withing a square and a heading, which is their current direction of movement.
The _traps_ consist of the pitfall traps and the wall constructions. They have a fixed position within a patch, catching radius, wall radius, and wall rotation.	

<table>
    <tr>
        <td> <b>Entity</b></td>
        <td><b>State variable</b></td>
        <td><b>Type</b></td>
        <td><b>Description</b></td>
    </tr>
    <tbody>
    <tr>
        <td>patch</td>
        <td>position</td>
        <td>2 integer</td>
        <td>x and y coordinate</td>
    </tr>
    <tr>
        <td></td>
        <td>is protected area?</td>
        <td>boolean</td>
        <td>Indicates if the patch is part of a protected area, if false the patch is a field patch</td>
    </tr>
    <tr>
        <td>beetles</td>
        <td>position</td>
        <td>2 floats</td>
        <td>x and y coordinate</td>
    </tr>
    <tr>
        <td></td>
        <td>heading</td>
        <td>float</td>
        <td>Direction that the beetle is facing in degree</td>
    </tr>
    <tr>
        <td>traps</td>
        <td>position</td>
        <td>2 floats</td>
        <td>x and y coordinate</td>
    </tr>
    <tr>
        <td></td>
        <td>catching radius</td>
        <td>float</td>
        <td>Distance to trap position in that beetles will be caught</td>
    </tr>
    <tr>
        <td></td>
        <td>wall radius</td>
        <td>float</td>
        <td>Radius of walls from trap position</td>
    </tr>
    <tr>
        <td></td>
        <td>wall rotation</td>
        <td>float</td>
        <td>Rotation of trap in degree</td>
    </tr>
    </tbody>
</table>

The spatial extent is 1 m per patch, the world size is 750 m x 750 m.
The temporal scale is 1 min per simulation step. Each simulation has 6300 steps.

## Process overview and scheduling
In each time step, the beetles will move in a random order. After one beetle has moved, it may get trapped and then the next beetle will move. More detail will be given in the Submodels section.


-       \caption{Model overview: In the initialization, an area of patches is declared as protected area (green). Around this area, the traps (orange) are setup, according to one of the four scenarios. The beetles' (red) position is either normally or uniformly distributed. 
-               When the model runs, the beetles move in a random order. For that, a step size is drawn from a gamma distribution. Then the beetle does either a biased step towards or away from the protected area, or a correlated step. In a correlated step the beetles turn based on with a normal distribution.
-               If the beetle has crossed a wall, its position and heading get corrected. Lastly, if the beetle has entered the trap's catching radius, the beetle gets counted and excluded from the model.
-               Inspired by [Milles2020]}

![figure:visual_ODD](../figures/visual_ODD.jpg)


## Design concepts
### Emergence
A trapping pattern, i.e. the count per pitfall trap, emerges from the interaction of beetles, trap walls, and pitfall traps. The movement behavior, represented as a correlated random walk, is imposed via fixed rules.
## Adaptation
Beetles can move with a bias. With a bias, they are attracted (positive bias) or repelled (negative) by the protected area. The bias encodes the probability, of making a step towards or away from the protected area. _Rationale:_ in various studies on beetles random, but also directed movement was observed [Wallin1988, Wallin1994, Allema2015, Bailey2020].The bias should encode the preference of one habitat [Wallin1988, Allema2019].
### Sensing
If there is a bias, the beetles know in what direction the protected area is.
### Interaction
The path of the beetle can be interrupted by a wall or a trap.
If a beetle encounters a wall, it adjusts its heading. That means, it turns parallel to the wall. Further detail is given in the Submodel section.
If a beetle moves into the catching radius of a trap, it will be counted and removed from the model. Further detail is given in the Submodel section.
We do not assume interactions among beetles. 
### Stochasticity
There are multiple random processes:

	1. The step size of each beetle is drawn from a gamma distribution.
	2. The turning angle is normally distributed with mu = 0 and a variance as given by input, if the step is not biased.
	3. The absolute bias gives the probability of making a step towards the protected area or, if the bias is negative, away from the protected area.

### Observation
As in the real field experiment, the trapping pattern is measured, i.e. the number of beetles per pitfall trap position. There are six positions, resulting from two rows with traps inwards, outwards and on the circle. Additionally, the net displacement can be measured by saving the starting position of the beetle.
## Initialization
For each simulation, the trap scenario and the beetles' initial position must be defined. The world size, number of beetles and number of time steps is constant.

<table>
    <tr>
        <td><b>Name</b></td>
        <td><b>Type</b></td>
        <td><b>Range</b></td>
        <td><b>Description</b></td>
    </tr>
    <tr>
        <td>world size</td>
        <td>integer</td>
        <td>750 (const.)</td>
        <td>length and height of the simulated world</td>
    </tr>
    <tr>
        <td>trap scenario</td>
        <td>string</td>
        <td>\{original, higher distance, same circular arc, optimized offset\}</td>
        <td>category of the traps' position</td>
    </tr>
    <tr>
        <td>n beetles</td>
        <td>integer</td>
        <td>1000 (const.)</td>
        <td>number of beetles per simulation</td>
    </tr>
    <tr>
        <td>init beetles</td>
        <td>string</td>
        <td>\{normal, uniform\}</td>
        <td>distribution of beetles' initial position</td>
    </tr>
    <tr>
        <td>init sigma</td>
        <td>float</td>
        <td>[0, infinity [</td>
        <td>standard deviation of beetles' initial position, if normally distributed</td>
    </tr>
    <tr>
        <td>number of time steps</td>
        <td>integer</td>
        <td>6300(const.)</td>
        <td>number of steps per simulation</td>
    </tr>
</table>


### World size
_Rationale:_
The world size must be big enough to ensure that beetles at the border of the world cannot reach a trap during the run time. In that way the edge behavior, i.e. if the world wraps or if the beetles will not move beyond the edge of the world, is irrelevant for the observation.
For measuring the sufficient world size, an experiment was created where beetles are initialized at the border and walk a random walk (bias = 0, other movement parameters vary). If one of the beetles reaches a traps, the world is not big enough.
%Since in the following experiments the beetles are usually initialized in the middle of the world, they would have to walk twice that distance.

### Initialization of the protected area
The protected area is set up as an area in the middle of the world by setting the variable _is protected area?_ of all rectangular patches with a distance smaller than 6 towards the middle patch to true. _Rationale:_ Real kettle holes usually cover less than 100 m^2 [Savic2021]. With this initialization, 109 patches and thus 109 m^2 belong to the protected area. This is a simplification and ignores the water body of the kettle hole for two reasons: firstly, the behavior of the beetles in the protected area is not subject to the study, and secondly most beetle species can swim [Andersen1968] and therefore cross the kettle hole.
### Initialization of traps
The directional traps consisting of walls and pitfall traps can be setup in four different scenarios. In the original experiment eight traps were set up in two rows resulting in 32 pitfall traps.
Pitfall traps with a radius of 6.5 cm were set in each corner of the cross shape walls. The walls have a radius of 1 m.
In the model the trap geometry is simplified with a catching radius: any beetle that is closer than 7.85 cm to the trap center is caught.	 

<img src="../figures/trap_positions.png" alt="trap_positions" width="500"/>

In a corner of the walls (orange) the beetles will be counted and removed from the simulation, if they cross the red, dotted line. This is a simplification of the real setup where a circular pitfall trap (black) with a diameter of 6.5 cm was installed.
<img src="../figures/trap_quadrant.png" alt="trap_quadrant" width="300"/>

#### Scenarios for trap setup

  * original
    * First row traps are setup with 1 m distance from the protected area,  second row traps with 8 m distance with a 2 m offset of the first trap parallel to the protected area border
  * higher distance
    * As original but with the second row traps having a higher distance from protected area (15 m)
  * same circular arc
    * As original but second row traps have a bigger wall radius (2.166174 m instead of 1 m)
    * **Rationale:** In the original setup, the second row traps have the same wall radius and are thus covering a smaller fraction of the arc, than the first row traps. With this setup they cover the same fraction of the arc length as the first row traps.
  * optimized offset
    * As original but second row traps have a maximum offset, hence they are in the middle of two first row traps 
    * **Rationale:** There migth be a "shadow" effect of the first row traps on the second row traps.

### Initialization of beetles
The beetles' position can be set either uniformly or normally distributed. If they are initiated with a normally distributed position, they are set at a position with x and y coordinate drawn from a normal distribution with mu = 0 and  sigma as given by user.
The heading of each beetle is set randomly. Per experiment 1000 beetles were initialized.
_Rationale:_
The pitfall traps are known to measure activity, not abundance [Greenslade1964]. There can be more abundant, but inactive species which are less likely to be caught in the traps. Because the abundance is unknown, not the absolute but the relative count of beetles per trap will be compared. Therefore, the absolute number of beetles per simulation is fixed to 1000 beetles. 

Since the beetle species cannot survive tillage and hibernate in the kettle hole area [Lövei1996, Hossain2002, Galle2018, French2001], the beetles spread from the kettle hole area into the field in spring. This is considered in the model by initializing the beetles normally distributed around the center of the kettle hole (i.e. mu = 0) with a standard deviation sigma = 3 m. Since the simulated kettle hole area has a radius of 6 m = 2 * sigma approximately 95 \%
of the beetles are initialized in the kettle hole area as P[mu -2 sigma <= X <= mu + 2 *sigma] = 0.9545.
This initialization is compared to a normal distribution with sigma = 10, where beetles have already spread into the fields, and a uniform initial distribution.

### Time step
_Rationale:_
Since the pitfall traps are emptied at the end of three weeks, the exact time when the beetles fell into the trap is unknown. Therefore, only the sum of the beetles' activity is relevant for the model, not their resting behavior.\\
The length of activity is based on the observation of three beetle species (_Poecilus cupreus_, _Pterostichus melanarius_ and _Pterostichus niger_) from [Firle1998]. 
In the experiments, the beetles were active for eight hours per day.
Within the eight hours they also rest, leading to the average resting probability of 78.25 - 80.26 % and activity probability of 19.74 - 21.75 % [Firle1998]. 
This results in an average of 284 - 313 minutes of activity per day. For simplification the activity is set to 300 minutes per day and therefore, because the step size is given in m/min, 300 steps per day.
Since the real experiments ran for 21 days, the simulation runs for 21 * 300 = 6300 steps.
## Input data
The model does not use input data to represent time-varying processes.
## Submodels
### Move
There are four global variables for movement: _step size mean_, _step size variance_, _standard deviation of the turning angle_ and a _bias_. The bias represents the probability of turning towards or away from the protected area and is added to express the preference of beetles towards a habitats.\\
First, the step size for the beetle is drawn from a gamma distribution Gamma(alpha, lambda).
Then the heading is set, based on the bias or the previous heading.
The absolute bias gives the probability of doing a biased step.
In a biased step, the beetles move towards to or away from the protected area, given a positive or negative bias respectively. If the beetle should do a step towards the protected area, it turns to the middle patch and thus towards to the protected area.
If the beetle should do a step away from the protected area, it turns 180° away from the middle patch and thus away the protected area.	
If the beetle should do a non biased step the beetle does a correlated random step. For a correlated random step, the beetles' turn with an angle drawn from a normal distribution with mu = 0 and  sigma as given by input. Because of the dependency of the new heading on the previous, this is called correlated.

In the program, only the positions of the traps exist, but not the walls. Therefore, whether or not a beetle would reach a wall and then turn direction needs to be calculated.
After the beetle has moved and if it is closer than 10 m from the closest trap, the quadrant and the closest wall of the beetle's position will be calculated. If it is further away, the quadrant and closest wall will not be calculated for run time reasons. The quadrant is one of the four areas, that are separated by the walls.
Knowing the closest wall and the quadrant of the beetle's position, it can be evaluated, if the beetle would have moved across a wall: if the closest trap is the same, the trap quadrant has changed and the beetle is or was within the wall radius, it will be set back towards it previous position, the intersection of its path and the wall will be calculated and the beetle will be set close to the wall.
After that, the beetle's heading will be set parallel to the wall. In that way, the beetle most likely moves along the wall but it can also escape the wall, if its heading changes in the next step. Details follow in the next subsection.

_Rationale:_
The gamma distribution was the most applicable step size distribution, since it is the maximum entropy distribution with two moment constrains and support on positive real numbers [Park2009]. Its parameters (shape alpha and a rate lambda) are calculated from a given mean mu and variance sigma^2.

Estimated from Fig. 2a in [Bailey2020], _Poecilus cupreus_ has a mean step size of 0.4 m/min with a variance of 0.1 m/min. The gamma distribution is a good fit for the measured data.


<img src="../figures/gamma_density_pcupreus.png" alt="step_size_dist" width="600"/>


### Calculation of the intersection of the beetle's path and the wall
Since the wall itself is mathematically represented as a line, the beetle cannot be set to the intersection of its path and the wall.
Instead, it is set close to the wall by moving 99 \% of the way from its current position towards the intersection of its path with the wall.
The beetles turn parallel to the wall (i.e. either getting the same heading or the wall's heading turned by 180°) facing the direction, which is closer towards its previous heading.

### Get trapped
After the beetle has done its final step, it will sense again its closest trap and if its distance towards it is smaller than the catching radius. In that case, the trap's count of caught beetles will be increased by one for the respective quadrant and the beetle is excluded from further simulation steps.

# Bibliography

**Allema2019** Allema, B., Hemerik, L., Rossing, W. A., Groot, J. C., van
Lenteren, J. C., and van der Werf, W. (2019). Dispersal of a carabid beetle
in farmland is driven by habitat-specific motility and preference at habitat
interfaces. Entomologia Experimentalis et Applicata, 167(8):741–754.

**Allema2015** Allema, B., van der Werf, W., Groot, J. C., Hemerik, L.,
Gort, G., Rossing, W. A., and van Lenteren, J. C. (2015). Quantification of
motility of carabid beetles in farmland. Bulletin of Entomological Research,
105(2):234–244.

**Andersen1968** Andersen, J. (1968). The effect of inundation and choice of
hibernation sites of coleoptera living on river banks. Norsk Entomologisk
Tidsskrift, 15(2):115–133.

**Bailey2020** Bailey, J. D., Benefer, C. M., Blackshaw, R. P., and
Codling, E. A. (2020). Walking behaviour in the ground beetle, poecilus
cupreus: dispersal potential, intermittency and individual variation. Bulletin
of Entomological Research, 111(2):200–209.

**Firle1998** Firle, S., Bommarco, R., Ekbom, B., and Natiello, M. (1998).
The influence of movement and resting behavior on the range of three carabid
beetles. Ecology, 79(6):2113–2122.

**French2001** French, B. W., Elliott, N. C., Berberet, R. C., and Burd,
J. D. (2001). Effects of riparian and grassland habitats on ground beetle
(coleoptera: Carabidae) assemblages in adjacent wheat fields. Environmental
Entomology, 30(2):225–234.

**Gallé2018** Gallé, R., Császár, P., Makra, T., Gallé-Szpisjak, N.,
Ladányi, Z., Torma, A., Ingle, K., and Szilassi, P. (2018). Small-scale agri-
cultural landscapes promote spider and ground beetle densities by offering
suitable overwintering sites. Landscape Ecology, 33(8):1435–1446.

**Greenslade1964** Greenslade, P. J. M. (1964). Pitfall trapping as a method
for studying populations of carabidae (coleoptera). The Journal of Animal
Ecology, 33(2):301.

**Grimm2006** Grimm, V., Berger, U., Bastiansen, F., Eliassen, S., Ginot,
V., Giske, J., Goss-Custard, J., Grand, T., Heinz, S. K., Huse, G., Huth, A.,
Jepsen, J. U., Jørgensen, C., Mooij, W. M., Müller, B., Pe’er, G., Piou, C.,
Railsback, S. F., Robbins, A. M., Robbins, M. M., Rossmanith, E., Rüger, N.,
Strand, E., Souissi, S., Stillman, R. A., Vabø, R., Visser, U., and DeAngelis,
D. L. (2006). A standard protocol for describing individual-based and agent-
based models. Ecological Modelling, 198(1-2):115–126.

**Grimm2020** Grimm, V., Railsback, S. F., Vincenot, C. E., Berger, U.,
Gallagher, C., DeAngelis, D. L., Edmonds, B., Ge, J., Giske, J., Groeneveld,
J., Johnston, A. S., Milles, A., Nabe-Nielsen, J., Polhill, J. G., Radchuk, V.,
Rohwäder, M.-S., Stillman, R. A., Thiele, J. C., and Ayllón, D. (2020). The
ODD protocol for describing agent-based and other simulation models: A
second update to improve clarity, replication, and structural realism. Journal
of Artificial Societies and Social Simulation, 23(2).

**Hossain2002** Hossain, Z., Gurr, G. M., Wratten, S. D., and Raman,
A. (2002). Habitat manipulation in lucerne medicago sativa: arthropod pop-
ulation dynamics in harvested and 'refuge' crop strips. Journal of Applied
Ecology, 39(3):445–454.

**Lövei1996** Lövei, G. L. and Sunderland, K. D. (1996). Ecol-
ogy and behavior of ground beetles (coleoptera: Carabidae). Annual Review
of Entomology, 41(1):231–256.

**Milles2020** Milles, A., Dammhahn, M., and Grimm, V. (2020). In-
traspecific trait variation in personality-related movement behavior promotes
coexistence. Oikos, 129(10):1441–1454.

**Park2009** Park, S. Y. and Bera, A. K. (2009). Maximum entropy
autoregressive conditional heteroskedasticity model. Journal of Econometrics,
150(2):219–230.

**Savić2021** Savić, B., Evgrafova, A., Donmez, C., Vasić, F., Glemnitz,
M., and Paul, C. (2021). Assessing the role of kettle holes for providing and
connecting amphibian habitats in agricultural landscapes. Land, 10(7):692.

**Wallin1994** Wallin, H. and Ekbom, B. (1994). Influence of
hunger level and prey densities on movement patterns in three species of
pterostichus beetles (coleoptera: Carabidae). Environmental entomology,
23(5):1171–1181.

**Wallin1988** Wallin, H. and Ekbom, B. S. (1988). Movements of
carabid beetles (coleoptera: Carabidae) inhabiting cereal fields: a field tracing
study. Oecologia, 77(1):39–43.

**Wilensky1999** Wilensky, U. (1999). NetLogo. Center for Connected Learning
and Computer-Based Modeling, Northwestern University, Evanston, IL.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="parameter_search_test" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;original&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step_size_mean">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step_size_variance">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="turning_angle_sd" first="5" step="5" last="10"/>
    <enumeratedValueSet variable="bias">
      <value value="-1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameter_search_higherDistance_sd" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;higher_distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="init_sd" first="3" step="3" last="12"/>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_sameCircularArc_sd" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;same_circular_arc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="init_sd" first="3" step="3" last="12"/>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_optimalOffset_sd" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;optimal_offset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="init_sd" first="3" step="3" last="12"/>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_original" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;original&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_higherDistance_random" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;higher_distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_sameCircularArc_random" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;same_circular_arc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_optimalOffset_random" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;optimal_offset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_original_random" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;original&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_original_sd" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;original&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="init_sd" first="3" step="3" last="12"/>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_optimalOffset" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;optimal_offset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_sameCircularArc" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;same_circular_arc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
  <experiment name="parameter_search_higherDistance" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[report_pitfall_list] of pitfall_traps</metric>
    <enumeratedValueSet variable="max_ticks">
      <value value="6300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldsize">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_beetles">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include_traps?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trap_scenario">
      <value value="&quot;higher_distance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_beetles">
      <value value="&quot;normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init_sd">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video_name">
      <value value="&quot;bias-0.05&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="step_size_mean" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="step_size_variance" first="0.1" step="0.1" last="0.5"/>
    <steppedValueSet variable="turning_angle_sd" first="5" step="10" last="45"/>
    <steppedValueSet variable="bias" first="-1" step="0.5" last="1"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
