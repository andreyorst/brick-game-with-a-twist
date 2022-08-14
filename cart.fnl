;; title:   Brick Game?
;; author:  Andrey Listopadov
;; site:    andreyorst.gitlab.io
;; desc:    Entry for the 2022 Fennel game jam
;; license: MIT License
;; version: 0.1
;; script:  fennel
;; strict:  true

;;; Game state

(var state nil)
(var dt 0)
(var pt 0)

(local world
  {:width 128
   :height 118
   :x-offset 10
   :y-offset 10})

(local powerup-duration 2600)
(local powerup-tic 300)
(local ball-speed 100)
(local powerup-speed 55)
(local platform-speed 120)
(local boss-speed 160)
(local speed-mult 1.3)

;;; Math

(local {:max m/max
        :min m/min
        :abs m/abs
        :sqrt m/sqrt
        :random m/random
        :floor m/floor
        :randomseed m/randomseed}
  math)

(fn clamp [low x high]
  (if (< x low) low
      (< high x) high
      x))

(fn nan? [x]
  (not= x x))

(fn round [x]
  (/ (m/floor (* x 100)) 100))

(fn sign-of [x]
  (if (< x 0) -1 1))

;;; Utils

(fn world->screen-coordinates [[x y]]
  (values (and x (+ x world.x-offset))
          (and y (+ y world.y-offset))))

;;; Vector math

(fn vec2 [?x ?y]
  (if (not ?x) [0 0]
      (not ?y) [?x ?x]
      [?x ?y]))

(fn vec-sub [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(fn vec-add [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(fn vec-mul [[x0 y0] [x1 y1]]
  [(* x0 x1) (* y0 y1)])

(fn vec-length [[x y]]
  (m/sqrt (+ (^ x 2) (^ y 2))))

(fn dot [[x0 y0] [x1 y1]]
  (+ (* x0 x1) (* y0 y1)))

(fn norm [[x y &as v]]
  (let [len (vec-length v)]
    [(/ x len) (/ y len)]))

;;; Ball

(fn spawn-ball [state ?pos ?vel]
  (let [b {:radius (if state.active-powerups.big 8 4)
           :speed ball-speed
           :vel (and ?vel (norm ?vel))
           :pos ?pos}
        {:pos [x y] : width} state.platform]
    (when (not b.pos)
      (set b.pos [(+ x (/ width 2))
                  (- y b.radius)]))
    (when (not b.vel)
      (set b.vel [1 -1]))
    (tset state.balls b true)))

(fn draw-ball [state ball]
  (let [(x y) (world->screen-coordinates ball.pos)]
    (if state.active-powerups.big
        (do (spr 12 (- x ball.radius) (- y ball.radius) 0)
            (spr 13 x (- y ball.radius) 0)
            (spr 28 (- x ball.radius) y 0)
            (spr 29 x y 0))
        (spr 1 (- x ball.radius) (- y ball.radius) 0))))

(fn remove-ball [state b ?boss]
  (tset state.balls b nil)
  (if ?boss
      (set state.boss-health (- state.boss-health 50))
      (when (= nil (next state.balls))
        (set state.lives (- state.lives 1)))))

;;; Power-ups

(fn move-powerup [powerup]
  (set powerup.pos (vec-add powerup.pos (vec-mul powerup.vel (vec2 (* powerup.speed dt))))))

(fn random-powerup []
  (let [powerups [:len+ :len- :vel+ :vel- :three :big]]
    (. powerups (m/random (length powerups)))))

(fn draw-powerup [powerup]
  (let [(x y) (world->screen-coordinates powerup.pos)]
    (spr (match powerup.type
           :three 5
           :len+ 6
           :len- 7
           :vel+ 8
           :vel- 9
           :big 14)
         (- x powerup.radius)
         (- y powerup.radius)
         0)))

(fn spawn-powerup [state t {:pos [x y] : width}]
  (tset state.powerups
        {:pos [(+ x (/ width 2))
               (+ y (/ width 2))]
         :type t
         :radius 3
         :vel [0 1]
         :speed powerup-speed}
        true))

;;; Effects

(fn shake-screen [t]
  (set state.shake-time t))

(fn blink-boss-text [state]
  (set state.boss-start 600))

(fn play-extra-life-sound []
  (sfx 1 "e-5" 30 2))

;;; Twist

(fn spawn-boss [state {:pos [x y &as pos]}]
  (shake-screen 600)
  (blink-boss-text state)
  (music 0 -1 -1 false)
  (set state.boss-health 870)
  (set state.boss {:pos pos
                   :width 24
                   :height 8
                   :boss true})
  (let [boss-center [(+ x (/ state.boss.width 2)) (+ y (/ state.boss.height 2))]]
    (each [ball (pairs state.balls)]
      (let [pos ball.pos
            vel (norm (vec-sub pos boss-center))
            pos* (vec-add pos (vec-mul (vec2 12) vel))]
        (set state.explosion vel)
        (set ball.pos pos*)
        (set ball.vel vel)))))

;;; Score

(fn spawn-1up [state]
  (let [[x y] state.platform.pos
        x* (+ x (/ state.platform.width 2) -8)]
    (tset state.oneups {:pos [x* y]
                        :max-y (- y 20)} true)))

(fn draw-1up [{:pos [x y]} ?end]
  (spr (if ?end 76 60) x y 0)
  (spr (if ?end 77 61) (+ x 8) y 0))

(fn do-1ups [state]
  (each [{:pos [_ y] : max-y &as oneup} (pairs state.oneups)]
    (draw-1up oneup)
    (if (< max-y y)
        (tset oneup.pos 2 (- y (* 20 dt)))
        (do (draw-1up oneup true)
            (tset state.oneups oneup nil)))))

(fn inc-score [state points]
  (let [{: score : next-life-score} state
        score (+ score points)]
    (set state.score score)
    (when (> score next-life-score)
      (play-extra-life-sound)
      (spawn-1up state)
      (set state.next-life-score (+ next-life-score 1000))
      (set state.lives (clamp 0 (+ state.lives 1) 9)))))

;;; Blocks

(fn spawn-blocks [state]
  (let [width 16
        height 8
        blocks {}]
    (var cnt 0)
    (for [y 0 (- (* height 4) 1) height]
      (for [x 0 (- world.width 1) width]
        (let [health (- 4 (/ y height))
              block {:pos [x y] : width : height : health}]
          (when (<= (m/random 100) 30) ; 30% chance to drop a power-up
            (set block.powerup (random-powerup)))
          (set cnt (+ cnt 1))
          (tset blocks block true))))
    (set state.blocks blocks)
    (set state.block-cnt cnt)))

(fn draw-block [block]
  (let [(x y) (world->screen-coordinates block.pos)]
    (rect (+ x 1) (+ y 1) (- block.width 2) (- block.height 2) block.health)
    (spr 10 x y 0)
    (spr 11 (+ x 8) y 0)))

(fn draw-blocks [blocks]
  (each [b (pairs blocks)]
    (draw-block (doto b (tset :color b.health)))))

(fn remove-block [state block]
  (when (= 1 state.block-cnt)
    (spawn-boss state block))
  (inc-score state 90)
  (set state.block-cnt (- state.block-cnt 1))
  (tset state.blocks block nil)
  (when block.powerup
    (spawn-powerup state block.powerup block)))

;;; Sound

(local notes ["D" "E" "E" "E" "D"])

(fn rand-note [oct]
  (string.format "%s-%s" (. notes (m/random 5)) oct))

(fn play-collision-sound [state ?boss]
  (when state.active-powerups.big
    (shake-screen 20))
  (sfx 0
       (rand-note (if state.active-powerups.big 3 6))
       30 1 15
       2))

;;; Collisions

(fn reflect [vec normal]
  (let [n (norm normal)
        [x y] (vec-sub vec (vec-mul [2 2] (vec-mul n (vec2 (dot vec n)))))]
    [(if (< x 0)
         (clamp -1 x -0.3)
         (clamp 0.3 x 1))
     (if (< y 0)
         (clamp -1 y -0.3)
         (clamp 0.3 y 1))]))

(fn corner-normal [{: pos} corner]
  (vec-sub pos corner))

(fn collision-point [{:pos [x y]}
                     {:pos [bx by] :width w :height h}]
  [(m/max bx (m/min x (+ bx w)))
   (m/max by (m/min y (+ by h)))])

(fn collides? [{:pos [x y] :radius r &as ball} rect]
  (let [[cx cy] (collision-point ball rect)
        dx (- x cx)
        dy (- y cy)]
    (< (+ (^ dx 2) (^ dy 2)) (^ r 2))))

(fn normal [{:pos [x y] &as ball} {:pos [bx by] : width : height &as rect}]
  (let [[cx cy &as c] (collision-point ball rect)]
    (if (or (and (= cx bx) (= cy by))
            (and (= cx (+ bx width)) (= cy by))
            (and (= cx bx) (= cy (+ by height)))
            (and (= cx (+ bx width)) (= cy (+ by height))))
        (norm (corner-normal ball c))
        (and (<= bx cx (+ bx width))
             (not (< by y (+ by height))))
        (if (<= y by) [0 1] [0 -1])
        (and (<= by cy (+ by height))
             (not (< bx x (+ bx width))))
        (if (<= x bx) [1 0] [-1 0])
        (do (set ball.pos (vec-sub ball.pos [5 5]))
            [1 -1]))))

(fn collide-boss [state ball]
  (let [n (normal ball state.boss)
        vel (norm (reflect ball.vel n))]
    (set ball.vel vel)
    (inc-score state 10)
    (play-collision-sound state boss)
    (set state.boss-health
         (- state.boss-health
            (if state.active-powerups.big 30 15)))))

(fn collide-block [state ball block]
  (let [[x y] (norm (reflect ball.vel (normal ball block)))]
    (set block.health
         (- block.health (if state.active-powerups.big 2 1)))
    (inc-score state 10)
    (when (<= block.health 0)
      (remove-block state block))
    (if (or (nan? x) (nan? y))
        (set ball.vel [-1 -1])
        (set ball.vel [x y]))))

(fn collide-ball [state ball]
  (let [{:pos [x y] : radius :vel [vx vy &as vel]} ball]
    (when (collides? ball state.platform)
      (let [n (normal ball state.platform)
            vel (norm (reflect vel n))]
        (set ball.vel vel)))
    (when (and state.boss
               (collides? ball state.boss))
      (collide-boss state ball))
    (each [b (pairs state.blocks)]
      (when (collides? ball b)
        (collide-block state ball b)))
    ;; collide with world walls
    (when (<= (- x radius) 0)
      (set ball.vel [(m/abs vx) vy]))
    (when (<= world.width (+ x radius))
      (set ball.vel [(- (m/abs vx)) vy]))
    (when (<= (- y radius (if state.boss (- (* radius 2)) 0)) 0)
      (if state.boss
          (remove-ball state ball :boss)
          (set ball.vel [vx (m/abs vy)])))
    (when (<= world.height (- y radius radius))
      (remove-ball state ball))))

(fn move-ball [state ball]
  (when state.bounce
    (let [[vx vy &as vel] (match ball.vel
                            [x 0] (norm [x 0.4])
                            [0 y] (norm [0.4 y])
                            vel vel)
          pos ball.pos
          _ (set ball.pos (vec-add ball.pos (vec-mul vel (vec2 (* (+ ball.speed state.extra-speed) dt)))))
          _ (collide-ball state ball)
          [vx* vy*] ball.vel]
      (if state.explosion
          (do (set ball.vel state.explosion)
              (set state.explosion nil))
          (when (or (not= vx vx*) (not= vy vy*))
            (play-collision-sound state)
            (set ball.pos (vec-add pos (vec-mul vel (vec2 -1)))))))))

(fn random-velocity []
  (let [vel-x (match (- 3 (m/random 6))
                0 (m/random 3)
                vel vel)
        vel-y (match (- 3 (m/random 6))
                0 (m/random 3)
                vel vel)]
    (norm [vel-x vel-y])))

(fn inc-speed [state ?val]
  (set state.extra-speed (clamp 0 (+ state.extra-speed (* (or ?val 0.25) dt)) 150)))

(fn change-platform-len [state x]
  (let [platform state.platform
        new-width (clamp 16 (+ platform.width x) 32)]
    (when (not= new-width platform.width)
      (set platform.width new-width)
      (tset platform.pos 1 (clamp 0
                                  (- (. platform.pos 1) (/ x 2))
                                  (- world.width platform.width))))))

(fn activate-len-powerup [state t]
  (tset state.active-powerups t
        (match (. state.active-powerups t)
          val (- val powerup-duration)
          _ (do (change-platform-len state (match t :len+ 8 :len- -8))
                0))))

(fn inc-ball-size [state]
  (when (and (not state.active-powerups.big)
             (not state.bounce))
    (each [b (pairs state.balls)]
      (set b.pos (vec-add b.pos [0 -4]))))
  (set state.active-powerups.big
       (match state.active-powerups.big
         val (- val powerup-duration)
         _ 0)))

(fn collide-powerup [state powerup]
  (if (collides? powerup state.platform)
      (do (match powerup.type
            :len+ (activate-len-powerup state :len+)
            :len- (activate-len-powerup state :len-)
            :vel+ (inc-speed state 1000)
            :vel- (inc-speed state -1000)
            :big (inc-ball-size state)
            :three (match (next state.balls)
                     ball (do (spawn-ball state ball.pos (random-velocity))
                              (spawn-ball state ball.pos (random-velocity)))))
          (inc-score state 100)
          (tset state.powerups powerup nil))
      (< world.height (. powerup.pos 2))
      (tset state.powerups powerup nil)))

(fn strafe-balls [state block orig-pos step]
  (each [ball (pairs state.balls)]
    (let [{: radius : pos} ball
          [x* &as pos*]
          (vec-add pos [(+ step (if state.bounce (sign-of step) 0)) 0])]
      (when (or (collides? ball block)
                (not state.bounce))
        (if (or (< (- x* radius) 0)
                (< world.width (+ x* radius)))
            (set block.pos (vec-add orig-pos [(- (sign-of step)) 0]))
            (set ball.pos (vec-add pos* (if state.bounce [0 1] [0 0]))))))))

;; AI

(fn move-boss [state]
  (match (values state.boss (next state.balls))
    (boss ball)
    (let [step (* boss-speed dt)
          [x y &as pos] boss.pos
          [cx] (collision-point ball boss)
          [x*] (if (< cx (+ x 6))
                   (vec-sub pos [step 0])
                   (< (+ x (- boss.width 6)) cx)
                   (vec-add pos [step 0])
                   [x y])
          x* (clamp 0 x* (- world.width boss.width))]
      (set boss.pos [x* y])
      (when (and (not= x x*) state.bounce)
        (strafe-balls state boss [x y] step)))))

(fn draw-boss [state]
  (match state.boss
    boss (let [(x y) (world->screen-coordinates boss.pos)]
           (spr 44 x y 0)
           (spr 45 (+ x 8) y 0)
           (spr 46 (+ x 16) y 0))))

;;; Platform

(fn draw-platform [platform]
  (let [(x y) (world->screen-coordinates platform.pos)]
    (spr 2 x (- y 1) 0)
    (var j 8)
    (for [i 8 (- platform.width 8 1) 8]
      (spr 3 (+ x i) (- y 1) 0)
      (set j (+ j 8)))
    (spr 4 (+ x j) (- y 1) 0)))

(fn move-platform [state dir]
  (let [{:pos [px py] : width : step &as platform} state.platform
        step (* (if (= dir :left) (- platform-speed) platform-speed) dt)
        [px* &as pos] [(clamp 0 (+ px step) (- world.width width)) py]]
    (set state.platform.pos pos)
    (when (not= px px*)
      (strafe-balls state platform [px py] step))))

;;; Game

(fn do-size-powerup [state ball]
  (match state.active-powerups.big
    (where t (< t powerup-duration))
    (do (set ball.radius 8)
        (set state.active-powerups.big
             (+ state.active-powerups.big (* powerup-tic dt))))
    t (do (set state.active-powerups.big nil)
          (each [ball (pairs state.balls)]
            (set ball.radius 4)
            (when (not state.bounce)
              (set ball.pos (vec-add ball.pos [0 4])))))))

(fn do-balls [state]
  (each [ball (pairs state.balls)]
    (do-size-powerup state ball)
    (move-ball state ball)
    (draw-ball state ball)))

(fn do-powerups [state]
  (each [p (pairs state.powerups)]
    (collide-powerup state p)
    (move-powerup p)
    (draw-powerup p)))

(fn do-len-powerup [state p]
  (match (. state.active-powerups p)
    (where t (< powerup-duration t))
    (do (change-platform-len state (match p :len+ -8 :len- 8))
        (tset state.active-powerups p nil))
    t (tset state.active-powerups p (+ t (* powerup-tic dt)))))

(fn do-platform [state]
  (do-len-powerup state :len+)
  (do-len-powerup state :len-)
  (when (btn 2) (move-platform state :left))
  (when (btn 3) (move-platform state :right))
  (when (btn 0) (set state.bounce true))
  (draw-platform state.platform))

(fn do-boss [state]
  (move-boss state)
  (when (and state.boss
             (> state.boss-health 0)
             (<= (m/random 10000) 10))  ; <1% chance to drop a power-up
    (let [pos (. [[14 -5] [46 -5] [78 -5] [110 -5]] (m/random 4))]
      (spawn-powerup state (random-powerup) {:pos pos :width 1 :height 1})))
  (draw-boss state))

;;; Interface

(fn print-stats [state x y]
  (print "BRICK GAME (WITH A TWIST)" (+ x 8) (+ 2 y) 15)
  (print (string.format "LIVES: %s\nSCORE: %s\nBLOCKS: %s\nSPEED: %s"
                        state.lives
                        state.score
                        state.block-cnt
                        (round (/ (+ ball-speed state.extra-speed) 100)))
         (+ world.width 18 x)
         (+ 10 y)
         12)
  (print "LAUNCH\nBALL"
         (+ world.width 72 x)
         (+ 98 y)
         15)
  (print "MOVE"
         (+ world.width 17 x)
         (+ 114 y)
         15)
  (print "ACTIVE\nPOWERS"
         (+ world.width 72 x)
         (+ 56 y)
         15)
  (when state.boss
    (print "BOSS HP" (+ world.width 18 x) (+ 37 y) 12)
    (let [health (/ state.boss-health 10)]
      (rect (+ world.width 18 x) (+ 43 y) health 2
            (if (<= 60 health) 5
                (<= 20 health 60) 3
                (<= health 20) 2)))))

(local active-powerups [])

(fn contains? [t el]
  (accumulate [res false
               _ v (ipairs t)
               :until res]
    (= v el)))

(fn remove [t el]
  (var done false)
  (each [i v (ipairs t)]
    (when (= el v)
      (table.remove t i)
      (set done true))))

(fn draw-powerups [state p t x y]
  (if (and t (< t powerup-duration))
      (do (when (not (contains? active-powerups p))
            (table.insert active-powerups p))
          (each [i p (ipairs active-powerups)]
            (let [t (match p
                      6 (or state.active-powerups.len+ 0)
                      7 (or state.active-powerups.len- 0)
                      14 (or state.active-powerups.big 0))
                  [x y] [(+ 148 x) (+ 49 (* 9 i) y)]]
              (spr p x y 0)
              ((fn loop [len color]
                 (if (> len 32)
                     (do (rect (+ x 12) (+ y 2) 32 4 color)
                         (loop (- len 32) (- color 1)))
                     (rect (+ x 12) (+ y 2) len 4 color)))
               (/ (- powerup-duration t) 90) 5))))
      (remove active-powerups p)))

(fn print-powerups [state x y]
  (draw-powerups state 6 state.active-powerups.len+ x y)
  (draw-powerups state 7 state.active-powerups.len- x y)
  (draw-powerups state 14 state.active-powerups.big x y))

(fn draw-interface [state]
  (let [x (- world.x-offset 10)
        y (- world.y-offset 10)]
    (map 0 0 30 17 x y 1)
    (print-stats state x y)
    (print-powerups state x y)
    (when (btn 2) (do (spr 52 (+ (* 21 8) x) (+ (* 14 8) y) 0)
                      (spr 53 (+ (* 22 8) x) (+ (* 14 8) y) 0)
                      (spr 68 (+ (* 21 8) x) (+ (* 15 8) y) 0)
                      (spr 69 (+ (* 22 8) x) (+ (* 15 8) y) 0)))
    (when (btn 3) (do (spr 56 (+ (* 25 8) x) (+ (* 14 8) y) 0)
                      (spr 57 (+ (* 26 8) x) (+ (* 14 8) y) 0)
                      (spr 72 (+ (* 25 8) x) (+ (* 15 8) y) 0)
                      (spr 73 (+ (* 26 8) x) (+ (* 15 8) y) 0)))
    (when (btn 0) (do (spr 54 (+ (* 23 8) x) (+ (* 12 8) y) 0)
                      (spr 55 (+ (* 24 8) x) (+ (* 12 8) y) 0)
                      (spr 70 (+ (* 23 8) x) (+ (* 13 8) y) 0)
                      (spr 71 (+ (* 24 8) x) (+ (* 13 8) y) 0)))
    (when (btn 1) (do (spr 58 (+ (* 23 8) x) (+ (* 14 8) y) 0)
                      (spr 59 (+ (* 24 8) x) (+ (* 14 8) y) 0)
                      (spr 74 (+ (* 23 8) x) (+ (* 15 8) y) 0)
                      (spr 75 (+ (* 24 8) x) (+ (* 15 8) y) 0)))))

;;; Game loop

(fn _G.BOOT []
  (set state
       {:dt 0
        :pt 0
        :score 0
        :lives 5
        :next-life-score 1000
        :balls {}
        :blocks {}
        :powerups {}
        :block-cnt 0
        :extra-speed 0.0001
        :active-powerups {}
        :oneups {}
        :platform
        (let [p {:width 24
                 :height 6
                 :color 15
                 :step 15}]
          (set p.pos [(- (/ world.width 2) (/ p.width 2))
                      (- world.height 3 p.height)])
          p)})
  (m/randomseed (tstamp))
  (spawn-ball state)
  (spawn-blocks state)
  (cls 0)
  (draw-blocks state.blocks)
  (draw-platform state.platform)
  (draw-ball state (next state.balls)))

(fn do-blink-boss-text [state]
  (match state.boss-start
    (where t (< 0 t))
    (do (set state.boss-start (- t (* powerup-tic dt)))
        (if (= 0 (% (// t 10) 2))
            (print "     BOSS BATTLE" 24 64 2)
            (print "     BOSS BATTLE" 24 64 4)))
    t (set state.boss-start nil)))

(fn do-screen-shake [state]
  (match state.shake-time
    (where t (< 0 t))
    (do (set state.shake-time (- t (* powerup-tic dt)))
        (set world.x-offset (/ (+ 8000 (m/random 3000)) 1000))
        (set world.y-offset (/ (+ 9000 (m/random 3000)) 1000)))
    t (do (set state.shake-time nil)
          (set world.x-offset 10)
          (set world.y-offset 10))))

(fn _G.TIC []
  (let [t (time)]
    (set dt (* speed-mult (/ (- t pt) 1000)))
    (set pt t)
    (if (= nil (next state.balls))
        (if (= 0 state.lives)
            (do (print "     GAME OVER\n\npress A to retry" 29 61 15)
                (print "     GAME OVER\n\npress A to retry" 28 60 3)
                (when (btn 4) (reset)))
            (do (set state.bounce false)
                (set state.active-powerups.big nil)
                (spawn-ball state)))
        (and state.boss (<= state.boss-health 0))
        (do (print "      YOU WIN\n\npress A to retry" 29 61 15)
            (print "      YOU WIN\n\npress A to retry" 28 60 5)
            (when (btn 4) (reset)))
        (do (cls 0)
            (do-screen-shake state)
            (do-balls state)
            (do-powerups state)
            (do-platform state)
            (do-boss state)
            (draw-blocks state.blocks)
            (do-blink-boss-text state)
            (do-1ups state)
            (draw-interface state)
            (when state.bounce
              (inc-speed state))))))

;; <TILES>
;; 001:00dddd000deeeed0deecdeeddedeeefddeeeeefddeeefffd0dffffd000dddd00
;; 002:000000000dddddddddccccd6eccccce6eccccce6ffddddf70fffffff00000000
;; 003:00000000dddddddd66666666666666666666666677777777ffffffff00000000
;; 004:00000000ddddddd06dccccdd6eccccde6ecccdde7fddddfffffffff000000000
;; 005:000000000555555055cc666656cd6cc756666cd766cc666766cd667707777770
;; 006:0000000005555550566c6666566c6667566c6667666c6667666ccc7707777770
;; 007:0000000004444440433c3333433c3332433c3332333c3332333ccc2202222220
;; 008:000000000444444043cccc3343c3333243cccc3233333c3233cccc2202222220
;; 009:000000000555555056cccc6656c6666756cccc6766666c6766cccc7707777770
;; 010:0cccccccc0000000c00ccc00c0c00000c0000000c0000000c00000000ddddddd
;; 011:ccccccc00000000d0000000d0000000d0000000d00000c0d0000000dddddddd0
;; 012:000000dd0000ddee000deeee00deeecd0deeecee0deedeeedeedeeeedeedeeee
;; 013:dd000000eedd0000eeeed000deeeed00eeeeeed0eeeeeed0eeeeeefdeeeeeefd
;; 014:000000000555555055ccc8665cddde875cddef876cdeef8766eff87707777770
;; 016:dddddddddfffffffdf111111df111111df111111df111111df111111df111111
;; 017:ddddddddffffffff111111111111111111111111111111111111111111111111
;; 018:ddddeeeefffdeeee11edfffe11edfffe11edfffe11edfffe11edfffe11edfffe
;; 019:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 020:eeeeeeeeeccccccceccccccceccdddddeccdddddeccdddfdeccddffdeccdffff
;; 021:eeeeeeeeccccccfecccccffedddddffedddddffedddddffedddddffefffddffe
;; 022:eeeeeeeeeccccccceccccccceccdddddeccddddfeccdddffeccddfffeccddddf
;; 023:eeeeeeeeccccccfecccccffedddddffefddddffeffdddffefffddffefddddffe
;; 024:eeeeeeeeeccccccceccccccceccdddddeccdddddeccdddddeccdddddeccdffff
;; 025:eeeeeeeeccccccfecccccffedddddffedddddffefddddffeffdddffefffddffe
;; 026:eeeeeeeeeccccccceccccccceccdddddeccddddfeccddddfeccddddfeccddddf
;; 027:eeeeeeeeccccccfecccccffedddddffefddddffefddddffefddddffefddddffe
;; 028:deeeeeeedeeeeeee0deeeeee0deeeeee00deeeef000dffff0000ddff000000dd
;; 029:eeeeeffdeeeefffdeeefffd0eeffffd0fffffd00ffffd000ffdd0000dd000000
;; 032:df111111df111111df111111df111111df111111df111111df111111df111111
;; 033:1111111111111111111111111111111111111111111111111111111111111111
;; 034:11edfffe11edfffe11edfffe11edfffe11edfffe11edfffe11edfffe11edfffe
;; 036:eccdffffeccddffdeccdddfdeccdddddeccdddddeccfffffecffffffeeeeeeee
;; 037:fffddffedddddffedddddffedddddffedddddffefffffffefffffffeeeeeeeee
;; 038:eccddddfeccddddfeccddddfeccdddddeccdddddeccfffffecffffffeeeeeeee
;; 039:fddddffefddddffefddddffedddddffedddddffefffffffefffffffeeeeeeeee
;; 040:eccdffffeccdddddeccdddddeccdddddeccdddddeccfffffecffffffeeeeeeee
;; 041:fffddffeffdddffefddddffedddddffedddddffefffffffefffffffeeeeeeeee
;; 042:eccddfffeccdddffeccddddfeccdddddeccdddddeccfffffecffffffeeeeeeee
;; 043:fffddffeffdddffefddddffedddddffedddddffefffffffefffffffeeeeeeeee
;; 044:0dddddddd111111fd11cc1ffd1c1ffffd1111fffd11fffffd111ffff0eeeeeee
;; 045:ddddddddfccffccff22cc22ff23ff32fffffffffffccccfffcffffcfeeeeeeee
;; 046:ddddddd0ff11111dfffff11dfff1111dffff111dfffffc1effff111eeeeeeee0
;; 048:df111111df111111df111111df111111df111111df111111dfeeeeeedddddddd
;; 049:111111111111111111111111111111111111111111111111eeeeeeeedddddddd
;; 050:11edfffe11edfffe11edfffe11edfffe11edfffe11edfffeeeedfffeddddfffe
;; 052:eeeeeeeeecccccccecddddddecddddddecddddddecddddddecddddfdecdddffd
;; 053:eeeeeeeeccccccfeddddddfeddddddfeddddddfeddddddfeddddddfeddddddfe
;; 054:eeeeeeeeecccccccecddddddecddddddecddddddecdddddfecddddffecdddfff
;; 055:eeeeeeeeccccccfeddddddfeddddddfeddddddfefdddddfeffddddfefffdddfe
;; 056:eeeeeeeeecccccccecddddddecddddddecddddddecddddddecddddddecdddddd
;; 057:eeeeeeeeccccccfeddddddfeddddddfeddddddfeddddddfefdddddfeffddddfe
;; 058:eeeeeeeeecccccccecddddddecddddddecddddddecdddddfecdddddfecdddddf
;; 059:eeeeeeeeccccccfeddddddfeddddddfeddddddfefdddddfefdddddfefdddddfe
;; 060:0555555556c666665cc66c6656c66c6656c66c6666c66c666ccc66cc06677777
;; 061:5555550066666660c6ccc660c6c66c70c6c66c70c6cccc70c6c7777077777700
;; 064:eeffffffeeffffffeeffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 065:ffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 066:fffffffefffffffefffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 068:ecddffffecddffffecdddffdecddddfdecddddddecddddddecffffffeeeeeeee
;; 069:fffdddfefffdddfeddddddfeddddddfeddddddfeddddddfefffffffeeeeeeeee
;; 070:ecdddddfecdddddfecdddddfecdddddfecddddddecddddddecffffffeeeeeeee
;; 071:fdddddfefdddddfefdddddfefdddddfeddddddfeddddddfefffffffeeeeeeeee
;; 072:ecddffffecddffffecddddddecddddddecddddddecddddddecffffffeeeeeeee
;; 073:fffdddfefffdddfeffddddfefdddddfeddddddfeddddddfefffffffeeeeeeeee
;; 074:ecdddddfecdddfffecddddffecdddddfecddddddecddddddecffffffeeeeeeee
;; 075:fdddddfefffdddfeffddddfefdddddfeddddddfeddddddfefffffffeeeeeeeee
;; 076:0666666666d777776dd77d7767d77d7767d77d7777d77d777ddd77dd07788888
;; 077:6666660077777770d7ddd770d7d77d80d7d77d80d7dddd80d7d8888088888800
;; </TILES>

;; <MAP>
;; 000:313131313131313131313131313131313131313131313131313131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:310111111111111111111111111111111121011111111111111111111121000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 002:310212121212121212121212121212121222021212121212121212121222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 003:310212121212121212121212121212121222021212121212121212121222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 004:310212121212121212121212121212121222021212121212121212121222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 005:310212121212121212121212121212121222031313131313131313131323000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:310212121212121212121212121212121222041414141414141414141424000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:310212121212121212121212121212121222011111111111213131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:310212121212121212121212121212121222021212121212223131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 009:310212121212121212121212121212121222021212121212223131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 010:310212121212121212121212121212121222031313131313233131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 011:310212121212121212121212121212121222041414141414243131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 012:310212121212121212121212121212121222313131313161713131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 013:310212121212121212121212121212121222313131313162723131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 014:3102121212121212121212121212121212223131314151a1b18191313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:3103131313131313131313131313131313233131314252a2b28292313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:310414141414141414141414141414141424313131313131313131313131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <WAVES>
;; 000:2223456789abccccccccba9876543222
;; 001:3333456789abcccc3333456789abcccc
;; </WAVES>

;; <SFX>
;; 000:10801080104010001000100010001000200020003000400050006000700080009000b000c000d000d000e000e000e000e000e000f000f000f000f000a15000000000
;; 001:c0008000500030000000000000700070007000c000c0005000900090009000900090009000e000e000e000e000e000e010e020e050e080e0b0e0f0e0500000003800
;; 002:0100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001002f4000000000
;; </SFX>

;; <PATTERNS>
;; 000:b404260000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa1000881000661000441000331000221000111001ff1205404260000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa1000881000661000441000331000221000111001ff1205404260000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa100088100066100044100033100022100011100000100000000000000000000000000
;; 001:5404280000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa1000881000661000441000331000221000111001ff1205404280000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa1000881000661000441000331000221000111001ff1205404280000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa100088100066100044100033100022100011100000100000000000000000000000000
;; 002:c4042a0000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa1000881000661000441000331000221000111001ff120c4042a0000000000000000000000000000000000000000000000000ff1000dd1000cc1000aa1000881000661000441000331000221000111001ff120c4042a0000000000000000000000200000000000000000000000000ff1000dd1000cc1000aa100088100066100044100033100022100011100000100000000000000000000000000
;; 003:800004100020800004000000800004800004800004100000800004100000800004100000800004800004800004100020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </PATTERNS>

;; <TRACKS>
;; 000:1803000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006440ef
;; </TRACKS>

;; <SCREEN>
;; 000:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 001:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 002:eeeeeeeeffffeeffffeeffffeefffeeffeefeeeeeeffffeefffeeffeffefffffeeeeeefefeeefeffffeffffeffeefeeeeeefffeeeeeeffffefeeefeffffeeffffeffffefeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 003:eeeeeeeeffeefeffeefeeffeeffeefeffefeeeeeeffeeeeffeefefffffeffeeeeeeeefeefefefeeffeeeffeeffeefeeeeeffeefeeeeeeffeefefefeeffeefffeeeeffeeefeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 004:eeeeeeeeffffeeffeefeeffeeffeeeefffeeeeeeeffeffeffeefefffffeffffeeeeeefeefffffeeffeeeffeefffffeeeeeffeefeeeeeeffeefffffeeffeeefffeeeffeeefeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 005:eeeeeeeeffeefeffffeeeffeeffeefeffefeeeeeeffeefefffffefefefeffeeeeeeeefeefffffeeffeeeffeeffeefeeeeefffffeeeeeeffeefffffeeffeeeefffeeffeeefeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 006:eeeeeeeeffffeeffeefeffffeefffeeffeefeeeeeeffffeffeefefeeefefffffeeeeeefeffeffeffffeeffeeffeefeeeeeffeefeeeeeeffeeffeffeffffeffffeeeffeefeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 007:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 008:eeeeeeeeddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddeeeeddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddeeee
;; 009:eeeeeeeedffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdeeeedffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdeeee
;; 010:eeeeeeeedf0cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc0edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 011:eeeeeeeedfc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dedfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 012:eeeeeeeedfc44ccc444444444dc44ccc444444444dc44ccc444444444dc44ccc444444444dc44ccc444444444dc44ccc444444444dc44ccc444444444dc44ccc444444444dedfffedf00cc0000cccc0cc00c0ccccc00cccc0cc0000000cc0000000000000000000000000000000000000000000000edfffe
;; 013:eeeeeeeedfc4c444444444444dc4c444444444444dc4c444444444444dc4c444444444444dc4c444444444444dc4c444444444444dc4c444444444444dc4c444444444444dedfffedf00cc00000cc00cc00c0cc0000ccc000cc000000ccc0000000000000000000000000000000000000000000000edfffe
;; 014:eeeeeeeedfc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dedfffedf00cc00000cc00cc00c0cccc000ccc000000000cc0c0000000000000000000000000000000000000000000000edfffe
;; 015:eeeeeeeedfc444444444444c4dc444444444444c4dc444444444444c4dc444444444444c4dc444444444444c4dc444444444444c4dc444444444444c4dc444444444444c4dedfffedf00cc00000cc000ccc00cc000000ccc0cc00000ccccc000000000000000000000000000000000000000000000edfffe
;; 016:eeeeeeeedfc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dc44444444444444dedfffedf00ccccc0cccc000c000ccccc0cccc00cc00000000c0000000000000000000000000000000000000000000000edfffe
;; 017:eeeeeeeedf0dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd0edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 018:eeeeeeeedf0cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc0edfffedf000cccc00ccc000ccc00cccc00ccccc0cc000000ccc000cc000ccc0000000000000000000000000000000000edfffe
;; 019:eeeeeeeedfc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dedfffedf00ccc000cc00c0cc00c0cc00c0cc0000cc00000cc00c0ccc00cc0cc000000000000000000000000000000000edfffe
;; 020:eeeeeeeedfc33ccc333333333dc33ccc333333333dc33ccc333333333dc33ccc333333333dc33ccc333333333dc33ccc333333333dc33ccc333333333dc33ccc333333333dedfffedf000ccc00cc0000cc00c0cc00c0cccc0000000000cccc00cc00ccc0c000000000000000000000000000000000edfffe
;; 021:eeeeeeeedfc3c333333333333dc3c333333333333dc3c333333333333dc3c333333333333dc3c333333333333dc3c333333333333dc3c333333333333dc3c333333333333dedfffedf0000ccc0cc00c0cc00c0cccc00cc0000cc000000000c00cc00cc00c000000000000000000000000000000000edfffe
;; 022:eeeeeeeedfc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dedfffedf00cccc000ccc000ccc00cc00c0ccccc0cc000000ccc00cccc00ccc0000000000000000000000000000000000edfffe
;; 023:eeeeeeeedfc333333333333c3dc333333333333c3dc333333333333c3dc333333333333c3dc333333333333c3dc333333333333c3dc333333333333c3dc333333333333c3dedfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 024:eeeeeeeedfc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dc33333333333333dedfffedf00cccc00cc00000ccc000ccc00cc00c00cccc0cc00000cccc00ccccc00000000000000000000000000000000edfffe
;; 025:eeeeeeeedf0dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd0edfffedf00cc00c0cc0000cc00c0cc00c0cc0c00ccc000cc00000000cc0000cc00000000000000000000000000000000edfffe
;; 026:eeeeeeeedf0cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc00cccccccccccccc000000000000000000cccccccccccccc00cccccccccccccc0edfffedf00cccc00cc0000cc00c0cc0000ccc0000ccc0000000000ccc0000cc000000000000000000000000000000000edfffe
;; 027:eeeeeeeedfc22222222222222dc22222222222222dc22222222222222dc22222222222222dc22222222222222d0000000000000000c22222222222222dc22222222222222dedfffedf00cc00c0cc0000cc00c0cc00c0cc0c0000ccc0cc00000cc0000c00cc00000000000000000000000000000000edfffe
;; 028:eeeeeeeedfc22ccc222222222dc22ccc222222222dc22ccc222222222dc22ccc222222222dc22ccc222222222d0000000000000000c22ccc222222222dc22ccc222222222dedfffedf00cccc00ccccc00ccc000ccc00cc00c0cccc00cc00000ccccc00ccc000000000000000000000000000000000edfffe
;; 029:eeeeeeeedfc2c222222222222dc2c222222222222dc2c222222222222dc2c222222222222dc2c222222222222d0000000000000000c2c222222222222dc2c222222222222dedfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 030:eeeeeeeedfc22222222222222dc22222222222222dc22222222222222dc22222222222222dc22222222222222d0000000000000000c22222222222222dc22222222222222dedfffedf000cccc0cccc00ccccc0ccccc0cccc00cc00000cccc000000cc0000000000000000000000000000000000000edfffe
;; 031:eeeeeeeedfc222222222222c2dc222222222222c2dc222222222222c2dc222222222222c2dc222222222222c2d0000000000000000c222222222222c2dc222222222222c2dedfffedf00ccc000cc00c0cc0000cc0000cc00c0cc00000000cc0000ccc0000000000000000000000000000000000000edfffe
;; 032:eeeeeeeedfc22222222222222dc22222222222222dc22222222222222dc22222222222222dc22222222222222d0000000000000000c22222222222222dc22222222222222dedfffedf000ccc00cc00c0cccc00cccc00cc00c000000000ccc000000cc0000000000000000000000000000000000000edfffe
;; 033:eeeeeeeedf0dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd00dddddddddddddd000000000000000000dddddddddddddd00dddddddddddddd0edfffedf0000ccc0cccc00cc0000cc0000cc00c0cc00000cc0000cc00cc0000000000000000000000000000000000000edfffe
;; 034:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf00cccc00cc0000ccccc0ccccc0cccc00cc00000ccccc0cc0cccc000000000000000000000000000000000000edfffe
;; 035:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 036:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 037:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 038:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 039:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 040:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 041:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 042:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 043:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 044:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 045:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffe
;; 046:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedfeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedfffe
;; 047:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddfffe
;; 048:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe
;; 049:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe
;; 050:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe
;; 051:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 052:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 053:eeeeeeeedf00000000000000000000000000000000000000000000000000000044444400000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 054:eeeeeeeedf0000000000000000000000000000000000000000000000000000043cccc330000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 055:eeeeeeeedf0000000000000000000000000000000000000000000000000000043c333320000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 056:eeeeeeeedf0000000000000000000000000000000000000000000000000000043cccc320000000000000000000000000000000000000000000000000000000000000000000edfffeddddddddddddddddddddddddddddddddddddddddddddddddddddeeeeefffeeefffeeffffeffffeffeefefffffeeeeeee
;; 057:eeeeeeeedf0000000000000000000000000000000000000000dddd00000000033333c320000000000000000000000000000000000000000000000000000000000000000000edfffedffffffffffffffffffffffffffffffffffffffffffffffffffdeeeeffeefeffeefeeffeeeffeeffeefeffeeeeeeeeee
;; 058:eeeeeeeedf000000000000000000000000000000000000000deeeed0000000033cccc220000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeffeefeffeeeeeffeeeffeeffeefeffffeeeeeeee
;; 059:eeeeeeeedf00000000000000000000000000000000000000deecdeed0000000022222200000000000000000000000000000000000000000000000000000000000000000000edfffedf000555555000000000000000000000000000000000000000edfffefffffeffeefeeffeeeffeeefffeeffeeeeeeeeee
;; 060:eeeeeeeedf00000000000000000000000000000000000000dedeeefd0000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf00566c666600005555555550000000000000000000000000edfffeffeefeefffeeeffeeffffeeefeeefffffeeeeeee
;; 061:eeeeeeeedf00000000000000000000000000000000000000deeeeefd0000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf00566c666700005555555550000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 062:eeeeeeeedf00000000000000000000000000000000000000deeefffd0000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf00566c666700005555555550000000000000000000000000edfffeffffeeefffeefeeefefffffeffffeeeffffeeeee
;; 063:eeeeeeeedf000000000000000000000000000000000000000dffffd00000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf00666c666700005555555550000000000000000000000000edfffeffeefeffeefefefefeffeeeeffeefefffeeeeeee
;; 064:eeeeeeeedf0000000000000000000000000000000000000000dddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf00666ccc7700000000000000000000000000000000000000edfffeffeefeffeefefffffeffffeeffeefeefffeeeeee
;; 065:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000777777000000000000000000000000000000000000000edfffeffffeeffeefefffffeffeeeeffffeeeefffeeeee
;; 066:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeffeeeeefffeeffeffefffffeffeefeffffeeeeee
;; 067:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 068:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 069:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 070:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 071:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 072:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 073:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 074:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 075:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 076:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 077:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 078:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 079:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 080:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 081:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 082:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 083:eeeeeeeedf0000000000000000000000000000000000000000000000000000000000dddd000000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 084:eeeeeeeedf000000000000000000000000000000000000000000000000000000000deeeed00000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 085:eeeeeeeedf00000000000000000000000000000000000000000000000000000000deecdeed0000000000000000000000000000000000000000000000000000000000000000edfffedf000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 086:eeeeeeeedf00000000000000000000000000000000000000000000000000000000dedeeefd0000000000000000000000000000000000000000000000000000000000000000edfffedfeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 087:eeeeeeeedf00000000000000000000000000000000000000000000000000000000deeeeefd0000000000000000000000000000000000000000000000000000000000000000edfffeddddddddddddddddddddddddddddddddddddddddddddddddddddfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 088:eeeeeeeedf00000000000000000000000000000000000000000000000000000000deeefffd0000000000000000000000000000000000000000000000000000000000000000edfffeeefffffffffffffffffffffffffffffffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 089:eeeeeeeedf000000000000000000000000000000000000000000000000000000000dffffd00000000000000000000000000000000000000000000000000000000000000000edfffeeefffffffffffffffffffffffffffffffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 090:eeeeeeeedf0000000000000000000000000000000000000000000000000000000000dddd000000000000000000000000000000000000000000000000000000000000000000edfffeeefffffffffffffffffffffffffffffffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 091:eeeeeeeedf0000000000000000000000000000000000000000000000000000000000000000000000000dddd000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 092:eeeeeeeedf000000000000000000000000000000000000000000000000000000000000000000000000deeeed00000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 093:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000deecdeed0000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 094:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000dedeeefd0000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 095:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000deeeeefd0000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 096:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000deeefffd0000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 097:eeeeeeeedf000000000000000000000000000000000000000000000000000000000000000000000000dffffd00000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeecccccccccccccfeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 098:eeeeeeeedf0000000000000000000000000000000000000000000000000000000000000000000000000dddd000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccccccccccccffeffeeeeefffeeffeefeffeefeefffeeffeefeeeee
;; 099:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddddddddffeffeeeeffeefeffeefefffefeffeefeffeefeeeee
;; 100:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddffddddffeffeeeeffeefeffeefefffffeffeeeefffffeeeee
;; 101:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccdddffffdddffeffeeeefffffeffeefeffeffeffeefeffeefeeeee
;; 102:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddffffffddffefffffeffeefeefffeeffeefeefffeeffeefeeeee
;; 103:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddffddddffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 104:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddffddddffeffffeeefffeeffeeeeffeeeeeeeeeeeeeeeeeeee
;; 105:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddffddddffeffeefeffeefeffeeeeffeeeeeeeeeeeeeeeeeeee
;; 106:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddffddddffeffffeeffeefeffeeeeffeeeeeeeeeeeeeeeeeeee
;; 107:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddddddddffeffeefefffffeffeeeeffeeeeeeeeeeeeeeeeeeee
;; 108:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccddddddddddffeffffeeffeefefffffefffffeeeeeeeeeeeeeeeee
;; 109:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeccffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 110:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeecfffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 111:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 112:eeeeeeeedf0000000000000000000000000000000000000000000fddddddddddddddddddddddddddddddf00000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 113:eeeeeeeedf0000000000000000000000000000000000000000000ddccccd666666666666666666dccccdd00000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeecccccccccccccfeecccccccccccccfeecccccccccccccfeeeeeeeeeeeeeeeeeeeeeeeee
;; 114:eeeeeeeedf0000000000000000000000000000000000000000000eccccce666666666666666666eccccde00000000000000000000000000000000000000000000000000000edfffeeffeffeefffeeffeefefffffeccccccccccccffeeccccccccccccffeeccccccccccccffeeeeeeeeeeeeeeeeeeeeeeeee
;; 115:eeeeeeeedf0000000000000000000000000000000000000000000eccccce666666666666666666ecccdde00000000000000000000000000000000000000000000000000000edfffeefffffeffeefeffeefeffeeeeccddddddddddffeeccddddddddddffeeccddddddddddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 116:eeeeeeeedf0000000000000000000000000000000000000000000ffddddf777777777777777777fddddff00000000000000000000000000000000000000000000000000000edfffeefffffeffeefeffeefeffffeeccddddddddddffeeccddddffddddffeeccddddddddddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 117:eeeeeeeedf0000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000edfffeefefefeffeefeefffeeffeeeeccdddfddddddffeeccddddffddddffeeccdddddfddddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 118:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeefeeefeefffeeeefeeefffffeccddffddddddffeeccddddffddddffeeccdddddffdddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 119:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccdfffffffddffeeccddddffddddffeeccdfffffffddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 120:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccdfffffffddffeeccddffffffddffeeccdfffffffddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 121:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccddffddddddffeeccdddffffdddffeeccdddddffdddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 122:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccdddfddddddffeeccddddffddddffeeccdddddfddddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 123:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccddddddddddffeeccddddddddddffeeccddddddddddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 124:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccddddddddddffeeccddddddddddffeeccddddddddddffeeeeeeeeeeeeeeeeeeeeeeeee
;; 125:eeeeeeeedf00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000edfffeeeeeeeeeeeeeeeeeeeeeeeeeeccffffffffffffeeccffffffffffffeeccffffffffffffeeeeeeeeeeeeeeeeeeeeeeeee
;; 126:eeeeeeeedfeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedfffeeeeeeeeeeeeeeeeeeeeeeeeeecfffffffffffffeecfffffffffffffeecfffffffffffffeeeeeeeeeeeeeeeeeeeeeeeee
;; 127:eeeeeeeeddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddfffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 128:eeeeeeeeeefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 129:eeeeeeeeeefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 130:eeeeeeeeeefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 131:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 132:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 133:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 134:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; 135:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
;; </SCREEN>

;; <PALETTE>
;; 000:1a1c2c67275dbb3e53f97d57ffcd75b1f07042b7642f717929366f455dc94ba6f67deff7eee7eb9fb0c2626c863e3c57
;; </PALETTE>
