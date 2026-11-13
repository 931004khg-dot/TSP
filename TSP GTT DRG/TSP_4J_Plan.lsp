;;; ==========================================================================
;;; 파일명: TSP_4J_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part J)
;;; 역  할: 연속 CIP 본체/브래킷/앵커/배면 그라우팅 평면 작도 및 CIP 유틸리티 [tsp-draw-cip-elements-and-grout, tsp-draw-cip-plan-per-segment, tsp-find-next-dist-by-chord, tsp-make-segment-guideline, tsp-draw-cip-plan]
;;; 포함 함수: tsp-draw-cip-elements-and-grout, draw-bracket-poly, tsp-draw-cip-plan-per-segment, tsp-find-next-dist-by-chord, tsp-make-segment-guideline, tsp-draw-cip-plan
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-draw-cip-elements-and-grout : 산출된 CIP 배열을 기반으로 CIP/H-Pile/브래킷/앵커 및 배면 그라우팅 작도
;;;;==========================================================================
(defun tsp-draw-cip-elements-and-grout (vla-pl vla-path cip-layout-items hpile-flags corner-pts chain-pts chain-seg-indices orig-vertices n-verts is-closed full-loop max-path-len boundary-orient in-norm d cip-ctc h b hpile-block seg-idx seg-data grouting-dia mspace br-layer br-color /
                                        draw-bracket-poly i cip-center-list prev-target-index local-i local-anchor-i last-hpile-i last-anchor-i
                                        cur-h cur-b cur-hpile-block curr-pt is-corner earth-dir target-k inward-dir hpile-rot target-index
                                        cur-seg-data cur-hpile-idx cur-hpile-vals cur-interval-idx cur-interval cur-anchor-n-str cur-anchor-n
                                        gap-since-last-hpile gap-since-last-anchor draw-hpile-here ins-pt num-dia num-h num-b
                                        p-start-center p-end-center p1 p2 p3 p4 br-pl err-catch hatch-obj sa seg-hpile-count
                                        target-anchor-chord cur-d-on-path anchor-d pt-end end-param deriv ang anchor-pt
                                        pt-on-orig-anch orig-param-anch orig-deriv-anch anchor-in-norm anchor-earth-dir anchor-inward-dir
                                        target-index-anch cip-wale-spec cip-wale-vals cip-wale-h max-anchor-L max-anchor-ang max-anchor-free max-anchor-bond has-anchor
                                        cur-support-list seg-start-v-anch seg-end-v-anch seg-ang-anch seg-len-anch anch-proj-d local-anc-wale-h
                                        s-item s-iv s-w-vals cur-free cur-bond cur-ang cur-L cur-brace next-seg-real-idx next-brace brace-blocked
                                        c-upg-s has-upg-s c-upg-e has-upg-e manual-L-start manual-L-end seg-start-v seg-end-v seg-ang
                                        pt-data pt dx dy pt-d pt-perp is-brace-s is-brace-e p-start p-end d-from-start d-from-end limit-s limit-e
                                        b-r chk-b-h chk-edge snap-ctc anchor-blocked pt-base-temp anchor-half-width prev-seg-real-idx
                                        seg-prev-v adj-ang-start dist-to-adj-start prev-wall-thick prev-seg-data prev-timber-thick prev-spec prev-vals
                                        prev-wale-spec prev-wale-vals prev-inward-norm vec-p-x vec-p-y dot-val prev-long-dir long-proj-start
                                        seg-next-v adj-ang-end dist-to-adj-end next-wall-thick next-seg-data next-timber-thick next-spec next-vals
                                        next-wale-spec next-wale-vals next-inward-norm vec-p-x2 vec-p-y2 dot-val2 next-long-dir long-proj-end
                                        cos-val proj-free proj-bond proj-prot pt-base cur-plan-offset anchor-manual-angles m-ang anchor-final-outward
                                        pt-free-end pt-bond-end pt-h1 pt-h2 pt-h3 pt-h4 pt-anchor-start
                                        old-offsetgap cur-grouting-offset-idx cur-grouting-offset-custom actual-grout-offset cur-grouting-space-idx auto-grout-mode
                                        vla-grout-offset-list vla-grout-path grout-total-len grout-sample-inside-count ratio grout-sample-pt tmp-off
                                        grout-sample-fail-count pt-off pt-orig orig-param-test param-safe orig-deriv orig-tangent expected-offset-norm
                                        vec-expect-x vec-expect-y vec-actual-x vec-actual-y dot-prod max-grout-len grout-dist drawn-grout-pts
                                        cip-chain-pts cip-gap-total cip-gap-i cip-gap-p1 cip-gap-p2 cip-gap-mid cip-gap-s cip-gap-ang cip-gap-y
                                        cip-gap-ptA cip-gap-ptB cip-gap-near cip-gap-dA cip-gap-dB cip-gap-pt
                                        grout-pt p-on-orig p-param target-k-grout target-index-grout cur-grouting-custom cur-grouting-dia-idx cur-grouting-dia
                                        grout-space-val skip-grout rem-grout-len)

  ;; 닫힌 폴리라인 강제 생성
  (defun draw-bracket-poly (pts layer color / lst ent)
    (setq lst (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                    (cons 8 layer) (cons 62 color) (cons 90 (length pts)) '(70 . 1)))
    (foreach p pts (setq lst (append lst (list (list 10 (car p) (cadr p))))))
    (setq ent (entmakex lst))
    (if (not ent)
      (progn
        (setq lst (subst (cons 8 (getvar "CLAYER")) (assoc 8 lst) lst))
        (setq ent (entmakex lst))
      )
    )
    ent
  )

  ;; [3] 통합 작도 실행 루프 (앵커 포함)
  (setq i 0)
  (setq cip-center-list '())

  (setq prev-target-index -1)
  (setq local-i 0 local-anchor-i 0)
  (setq last-hpile-i -999 last-anchor-i -999)
  (setq cur-h h cur-b b cur-hpile-block hpile-block)

  (foreach cip-item cip-layout-items
    (setq curr-pt (nth 0 cip-item))
    (setq is-corner (nth 1 cip-item))
    (setq earth-dir (nth 2 cip-item))
    (setq target-k (nth 3 cip-item))
    (setq inward-dir (+ earth-dir pi))
    (setq hpile-rot (+ inward-dir (/ pi 2.0)))
    (setq target-index (nth target-k chain-seg-indices))
    (if (null target-index) (setq target-index seg-idx))

    ;; [실시간 세그먼트 파라미터 로드 및 스마트 카운터 보정]
    (if (/= target-index prev-target-index)
      (progn
        (setq cur-seg-data (nth target-index *segment-list*))

        ;; H-Pile 규격 실시간 업데이트
        (setq cur-hpile-idx (if (assoc 'CIP-HPILE-IDX cur-seg-data) (cdr (assoc 'CIP-HPILE-IDX cur-seg-data)) *tsp-cip-hpile-idx*))
        (setq cur-hpile-vals (parse-h-spec (nth (atoi cur-hpile-idx) *tsp-std-wall-list*)))
        (setq cur-h (nth 0 cur-hpile-vals) cur-b (nth 1 cur-hpile-vals))
        (setq cur-hpile-block (create-hpile-section-block cur-h cur-b (nth 2 cur-hpile-vals) (nth 3 cur-hpile-vals)))

        ;; 간격 실시간 업데이트
        (setq cur-interval-idx (if (assoc 'CIP-INTERVAL-IDX cur-seg-data) (cdr (assoc 'CIP-INTERVAL-IDX cur-seg-data)) *tsp-cip-interval-idx*))
        (setq cur-interval (cond ((= cur-interval-idx "1") 2) ((= cur-interval-idx "2") 3) (t 1)))

        (setq cur-anchor-n-str (if (assoc 'CIP-ANCHOR-N cur-seg-data) (cdr (assoc 'CIP-ANCHOR-N cur-seg-data)) (itoa (1+ (atoi (if cur-interval-idx cur-interval-idx "2"))))))
        (setq cur-anchor-n (if (and cur-anchor-n-str (/= cur-anchor-n-str "")) (atoi cur-anchor-n-str) 1))
        (if (<= cur-anchor-n 0) (setq cur-anchor-n 1))

        ;; 세그먼트 변경 시 물리적 코너 여부에 따른 카운팅 연속성 보정
        (if is-corner
          (progn
            (setq local-i 0 local-anchor-i 0)
          )
          (if (/= prev-target-index -1)
            (progn
              (setq local-i 0 local-anchor-i 0)

              (setq gap-since-last-hpile (- i last-hpile-i))
              (if (< gap-since-last-hpile cur-interval)
                (setq local-i gap-since-last-hpile)
              )

              (setq gap-since-last-anchor (- i last-anchor-i))
              (if (< gap-since-last-anchor cur-anchor-n)
                (setq local-anchor-i gap-since-last-anchor)
              )
            )
            (progn (setq local-i 0 local-anchor-i 0))
          )
        )
        (setq prev-target-index target-index)
      )
    )

    ;; 단일 코너 진입 시 리셋 (세그먼트 변경이 없는 일반 물리적 코너 대비)
    (if (and is-corner (= target-index prev-target-index))
      (progn
        (setq local-i 0 local-anchor-i 0)
      )
    )

    ;; [3.1] CIP 원형 콘크리트 감싸기 작도 (모서리/직선구간 공통)
    (entmake (list '(0 . "CIRCLE") (cons 8 "_CIP") (cons 62 2) (list 10 (car curr-pt) (cadr curr-pt) 0.0) (cons 40 (/ d 2.0))))
    (setq cip-center-list (append cip-center-list (list curr-pt)))

    ;; [3.2] H-Pile 작성 및 브래킷 생성 (사전 시뮬레이션 및 양끝단 1칸 이동 보정 플래그 반영)
    (setq draw-hpile-here (nth i hpile-flags))
    (if draw-hpile-here
      (progn
        (setq ins-pt (polar curr-pt inward-dir (/ cur-h 2.0)))
        (entmake (list '(0 . "INSERT") (cons 2 cur-hpile-block) '(8 . "_측면말뚝") (list 10 (car ins-pt) (cadr ins-pt) 0.0) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rot)))
        (setq *tsp-hpile-points* (append *tsp-hpile-points* (list (list (car ins-pt) (cadr ins-pt) 0.0))))

        ;; 브래킷 및 해치 작도
        (setq num-dia (if (= (type d) 'STR) (atof d) d))
        (setq num-h (if (= (type cur-h) 'STR) (atof cur-h) cur-h))
        (setq num-b (if (= (type cur-b) 'STR) (atof cur-b) cur-b))

        (if (> num-dia num-h)
          (progn
            (setq p-start-center (polar curr-pt inward-dir (/ num-h 2.0)))
            (setq p-end-center (polar curr-pt inward-dir (/ num-dia 2.0)))

            (setq p1 (polar p-start-center (+ inward-dir (/ pi 2.0)) (/ num-b 2.0)))
            (setq p2 (polar p-end-center (+ inward-dir (/ pi 2.0)) (/ num-b 2.0)))
            (setq p3 (polar p-end-center (- inward-dir (/ pi 2.0)) (/ num-b 2.0)))
            (setq p4 (polar p-start-center (- inward-dir (/ pi 2.0)) (/ num-b 2.0)))
            (setq br-pl (draw-bracket-poly (list p1 p2 p3 p4) br-layer br-color))

            (if br-pl
              (progn
                (setq err-catch (vl-catch-all-apply 'vla-addhatch (list mspace 0 "ANSI31" :vlax-true)))
                (if (not (vl-catch-all-error-p err-catch))
                  (progn
                    (setq hatch-obj err-catch)
                    (setq sa (vlax-make-safearray 9 (cons 0 0)))
                    (vlax-safearray-fill sa (list (vlax-ename->vla-object br-pl)))
                    (vla-appendouterloop hatch-obj sa)
                    (vla-evaluate hatch-obj)
                    (vla-put-patternscale hatch-obj (if (= (getvar "MEASUREMENT") 0) 400.0 20.0))
                    (vla-put-layer hatch-obj br-layer)
                    (vla-put-color hatch-obj br-color)
                  )
                )
              )
            )
          )
        )

        ;; H-Pile 개수 카운팅
        (setq seg-hpile-count (nth target-index *tsp-segment-pile-counts*))
        (if (null seg-hpile-count) (setq seg-hpile-count 0))
        (if (and (= target-index 0) (= seg-hpile-count 1) (= i 0))
          nil
          (setq *tsp-segment-pile-counts* (update-list-item *tsp-segment-pile-counts* target-index (1+ seg-hpile-count)))
        )
        (setq *tsp-actual-drawn-hpiles* (1+ *tsp-actual-drawn-hpiles*))
        (setq last-hpile-i i)
      )
    )

    ;; [3.3] 앵커 작도 (스마트 카운터 기준)
    (if (or is-corner (= (rem local-anchor-i cur-anchor-n) 0))
      (progn
        (setq target-anchor-chord
          (if (and (= cur-interval 3) (= (rem cur-anchor-n 3) 0))
            (* cip-ctc 1.5)
            (* cip-ctc 0.5)
          )
        )

        (setq cur-d-on-path (nth 4 cip-item))
        (setq anchor-d (+ cur-d-on-path target-anchor-chord))

        (if (and full-loop (> max-path-len 0.0))
          (while (>= anchor-d max-path-len) (setq anchor-d (- anchor-d max-path-len)))
        )

        (if (> anchor-d max-path-len)
          (progn
            (setq pt-end (vlax-curve-getPointAtDist vla-path max-path-len))
            (setq end-param (vlax-curve-getendparam vla-path))
            (setq deriv (vlax-curve-getfirstderiv vla-path end-param))
            (setq ang (atan (cadr deriv) (car deriv)))
            (setq anchor-pt (polar pt-end ang (- anchor-d max-path-len)))
          )
          (setq anchor-pt (vlax-curve-getPointAtDist vla-path anchor-d))
        )

        ;; 이동한 앵커 위치에서의 정확한 법선 벡터 재도출
        (setq pt-on-orig-anch (vlax-curve-getClosestPointTo vla-pl anchor-pt))
        (if pt-on-orig-anch
          (progn
            (setq orig-param-anch (vlax-curve-getParamAtPoint vla-pl pt-on-orig-anch))
            (setq orig-deriv-anch (vlax-curve-getFirstDeriv vla-pl orig-param-anch))
            (setq anchor-in-norm (+ (angle '(0 0 0) orig-deriv-anch) (* boundary-orient (/ pi 2.0))))
          )
          (setq anchor-in-norm in-norm)
        )
        (setq anchor-earth-dir (+ anchor-in-norm pi))
        (setq anchor-inward-dir (+ anchor-earth-dir pi))

        (setq target-index-anch target-index)

        (setq cip-wale-spec (cdr (assoc 'WALE-SPEC (nth target-index-anch *segment-list*))))
        (if (= cip-wale-spec "User-defined")
          (setq cip-wale-vals (cdr (assoc 'WALE-CUSTOM (nth target-index-anch *segment-list*))))
          (setq cip-wale-vals (parse-h-spec cip-wale-spec))
        )
        (if (null cip-wale-vals) (setq cip-wale-vals '(300 300 10 15)))
        (setq cip-wale-h (nth 0 cip-wale-vals))

        ;; [지보재 적용 구간 연동]
        (setq max-anchor-L 0.0 max-anchor-ang 0.0 max-anchor-free 0.0 max-anchor-bond 0.0 has-anchor nil)
        (setq cur-support-list (cdr (assoc 'SUPPORT-LIST (nth target-index-anch *segment-list*))))
        (setq seg-start-v-anch (nth target-index-anch orig-vertices))
        (setq seg-end-v-anch (nth (if is-closed (rem (+ target-index-anch 1) n-verts) (+ target-index-anch 1)) orig-vertices))
        (setq seg-ang-anch (angle seg-start-v-anch seg-end-v-anch))
        (setq seg-len-anch (distance seg-start-v-anch seg-end-v-anch))
        (setq anch-proj-d (+ (* (- (car anchor-pt) (car seg-start-v-anch)) (cos seg-ang-anch))
                             (* (- (cadr anchor-pt) (cadr seg-start-v-anch)) (sin seg-ang-anch))))
        (setq local-anc-wale-h 0)
        (if cur-support-list
          (foreach s-item cur-support-list
            (if (wcmatch (car s-item) "*앵커*")
              (progn
                (setq s-iv (tsp-get-support-item-dist-interval s-item seg-start-v-anch seg-ang-anch seg-len-anch))
                (if (and (>= anch-proj-d (- (car s-iv) (/ cur-b 2.0) 10.0))
                         (<= anch-proj-d (+ (cdr s-iv) (/ cur-b 2.0) 10.0)))
                  (progn
                    (setq s-w-vals (tsp-get-support-item-wale-vals s-item cip-wale-spec (cdr (assoc 'WALE-CUSTOM (nth target-index-anch *segment-list*)))))
                    (if (> (nth 0 s-w-vals) local-anc-wale-h) (setq local-anc-wale-h (nth 0 s-w-vals)))
                    (setq cur-free (nth 5 s-item) cur-bond (nth 6 s-item) cur-ang (nth 4 s-item))
                    (setq cur-L (+ cur-free cur-bond))
                    (if (> cur-L max-anchor-L)
                      (setq max-anchor-L cur-L max-anchor-ang cur-ang max-anchor-free cur-free max-anchor-bond cur-bond has-anchor T)
                    )
                  )
                )
              )
            )
          )
        )
        (if (> local-anc-wale-h 0) (setq cip-wale-h local-anc-wale-h))

        (setq cur-brace (cdr (assoc 'CORNER-BRACE (nth target-index-anch *segment-list*))))
        (setq next-seg-real-idx (if is-closed (rem (+ target-index-anch 1) n-verts) (+ target-index-anch 1)))
        (setq next-brace nil)
        (if (and next-seg-real-idx (< next-seg-real-idx (length *segment-list*)))
          (setq next-brace (cdr (assoc 'CORNER-BRACE (nth next-seg-real-idx *segment-list*))))
        )

        (setq brace-blocked nil)

        ;; 코너 부근 앵커 및 사보강재 간섭 검사 (안전거리 계산)
        (setq c-upg-s (cdr (assoc 'UPGRADE-WALE (nth target-index-anch *segment-list*))))
        (setq has-upg-s (and c-upg-s (= (car c-upg-s) "Y")))
        (setq c-upg-e (cdr (assoc 'UPGRADE-WALE (nth next-seg-real-idx *segment-list*))))
        (setq has-upg-e (and c-upg-e (= (car c-upg-e) "Y")))

        (setq manual-L-start nil manual-L-end nil)
        (setq seg-start-v (nth target-index-anch orig-vertices))
        (setq seg-end-v (nth (if is-closed (rem (+ target-index-anch 1) n-verts) (+ target-index-anch 1)) orig-vertices))
        (setq seg-ang (angle seg-start-v seg-end-v))

        (if (and has-upg-s (> (length c-upg-s) 2))
          (foreach pt-data (nth 2 c-upg-s)
            (setq pt (cadr pt-data) dx (- (car pt) (car seg-start-v)) dy (- (cadr pt) (cadr seg-start-v)))
            (setq pt-d (+ (* dx (cos seg-ang)) (* dy (sin seg-ang))))
            (setq pt-perp (abs (- (* dy (cos seg-ang)) (* dx (sin seg-ang)))))
            (if (and (> pt-d 0) (< pt-perp 1000.0)) (setq manual-L-start pt-d))
          )
        )
        (if (and has-upg-e (> (length c-upg-e) 2))
          (foreach pt-data (nth 2 c-upg-e)
            (setq pt (cadr pt-data) dx (- (car pt) (car seg-end-v)) dy (- (cadr pt) (cadr seg-end-v)))
            (setq pt-d (+ (* dx (cos (+ seg-ang pi))) (* dy (sin (+ seg-ang pi)))))
            (setq pt-perp (abs (- (* dy (cos (+ seg-ang pi))) (* dx (sin (+ seg-ang pi))))))
            (if (and (> pt-d 0) (< pt-perp 1000.0)) (setq manual-L-end pt-d))
          )
        )

        (setq is-brace-s (and cur-brace (= (car cur-brace) "Y") (cadr cur-brace)))
        (setq is-brace-e (and next-brace (= (car next-brace) "Y") (cadr next-brace)))

        (setq p-start (if (and corner-pts (< target-k (length corner-pts))) (car (nth target-k corner-pts)) seg-start-v))
        (setq p-end (if (and corner-pts (< (1+ target-k) (length corner-pts))) (car (nth (1+ target-k) corner-pts)) seg-end-v))

        (setq d-from-start (min (if p-start (distance anchor-pt p-start) 1e99)
                                (distance pt-on-orig-anch seg-start-v)
                                (max 0.0 anch-proj-d)))
        (setq d-from-end (min (if p-end (distance anchor-pt p-end) 1e99)
                              (distance pt-on-orig-anch seg-end-v)
                              (max 0.0 (- seg-len-anch anch-proj-d))))
        (setq limit-s 0.0 limit-e 0.0)

        ;; 사보강재 물리적 간섭 범위 산정
        (if is-brace-s
          (foreach b-r (cadr cur-brace)
            (setq chk-b-h (nth 0 (cadr b-r)))
            (setq chk-edge (if (wcmatch (strcase (nth 0 b-r)) "2H*") (* chk-b-h 2.1213) (* chk-b-h 0.7071)))
            (if (> (+ (* (nth 2 b-r) 1000.0) chk-edge 150.0) limit-s)
              (setq limit-s (+ (* (nth 2 b-r) 1000.0) chk-edge 150.0))
            )
          )
        )
        (if is-brace-e
          (foreach b-r (cadr next-brace)
            (setq chk-b-h (nth 0 (cadr b-r)))
            (setq chk-edge (if (wcmatch (strcase (nth 0 b-r)) "2H*") (* chk-b-h 2.1213) (* chk-b-h 0.7071)))
            (if (> (+ (* (nth 2 b-r) 1000.0) chk-edge 150.0) limit-e)
              (setq limit-e (+ (* (nth 2 b-r) 1000.0) chk-edge 150.0))
            )
          )
        )

        ;; 상향 띠장이 스냅되는 다음 H-Pile 위치까지 앵커 차단 범위 확장
        (setq snap-ctc (* cip-ctc cur-interval))
        (if (> limit-s 0.0)
          (setq limit-s (+ limit-s snap-ctc (/ cur-b 2.0)))
        )
        (if (> limit-e 0.0)
          (setq limit-e (+ limit-e snap-ctc (/ cur-b 2.0)))
        )
        (if manual-L-start
          (setq limit-s (max limit-s (+ manual-L-start (/ cur-b 2.0) 100.0)))
          (if (and has-upg-s (<= limit-s 0.0))
            (setq limit-s (+ snap-ctc (/ cur-b 2.0) 100.0))
          )
        )
        (if manual-L-end
          (setq limit-e (max limit-e (+ manual-L-end (/ cur-b 2.0) 100.0)))
          (if (and has-upg-e (<= limit-e 0.0))
            (setq limit-e (+ snap-ctc (/ cur-b 2.0) 100.0))
          )
        )
        (if (and (or (and has-upg-s (> limit-s 0.0)) (and has-upg-e (> limit-e 0.0)))
                 (< (- seg-len-anch (+ (if has-upg-s limit-s 0.0) (if has-upg-e limit-e 0.0))) (* snap-ctc 1.5)))
          (setq limit-s seg-len-anch limit-e seg-len-anch)
        )

        (if (or (<= d-from-start limit-s) (<= d-from-end limit-e))
          (setq brace-blocked T)
        )

        (setq anchor-blocked brace-blocked)

        ;; 앵커 정착부 인접 벽체/띠장 파묻힘(간섭) 기하 검사
        (if (and has-anchor (not anchor-blocked))
          (progn
            (setq pt-base-temp (polar anchor-pt anchor-inward-dir (+ (/ d 2.0) cip-wale-h)))
            (setq anchor-half-width 150.0)

            ;; 1. 시작점 코너 방향 간섭 확인
            (setq prev-seg-real-idx (if is-closed (if (= target-index-anch 0) (- n-verts 1) (- target-index-anch 1)) (- target-index-anch 1)))
            (if (and p-start (>= prev-seg-real-idx 0) (< d-from-start 2000.0))
              (progn
                (setq seg-prev-v (nth prev-seg-real-idx orig-vertices))
                (if seg-prev-v
                  (progn
                    (setq adj-ang-start (angle seg-start-v seg-prev-v))
                    (setq dist-to-adj-start (abs (* (sin (- (angle seg-start-v pt-base-temp) adj-ang-start)) (distance seg-start-v pt-base-temp))))

                    (setq prev-wall-thick 150.0)
                    (setq prev-seg-data (nth prev-seg-real-idx *segment-list*))
                    (if (= (cdr (assoc 'WALL-TYPE prev-seg-data)) "CIP")
                      (setq prev-wall-thick (/ (atof (if (assoc 'CIP-DIA prev-seg-data) (cdr (assoc 'CIP-DIA prev-seg-data)) "400")) 2.0))
                      (progn
                        (setq prev-timber-thick (cdr (assoc 'TIMBER-THICKNESS prev-seg-data)))
                        (if (null prev-timber-thick) (setq prev-timber-thick 60))
                        (setq prev-spec (cdr (assoc 'WALL-SPEC prev-seg-data)))
                        (if (= prev-spec "User-defined") (setq prev-vals (cdr (assoc 'WALL-CUSTOM prev-seg-data))) (setq prev-vals (parse-h-spec prev-spec)))
                        (if prev-vals (setq prev-wall-thick (+ (nth 3 prev-vals) (/ prev-timber-thick 2.0))))
                      )
                    )
                    (setq prev-wale-spec (cdr (assoc 'WALE-SPEC prev-seg-data)))
                    (if (= prev-wale-spec "User-defined") (setq prev-wale-vals (cdr (assoc 'WALE-CUSTOM prev-seg-data))) (setq prev-wale-vals (parse-h-spec prev-wale-spec)))
                    (if (null prev-wale-vals) (setq prev-wale-vals '(300 300 10 15)))

                    (setq prev-inward-norm (+ (angle seg-prev-v seg-start-v) (* boundary-orient (/ pi 2.0))))
                    (setq vec-p-x (- (car pt-base-temp) (car seg-start-v)))
                    (setq vec-p-y (- (cadr pt-base-temp) (cadr seg-start-v)))
                    (setq dot-val (+ (* vec-p-x (cos prev-inward-norm)) (* vec-p-y (sin prev-inward-norm))))

                    (setq prev-long-dir (angle seg-start-v seg-prev-v))
                    (setq long-proj-start (+ (* vec-p-x (cos prev-long-dir)) (* vec-p-y (sin prev-long-dir))))

                    (if (and (> dot-val 0)
                             (> long-proj-start (- anchor-half-width))
                             (< dist-to-adj-start (+ prev-wall-thick (nth 0 prev-wale-vals) anchor-half-width)))
                      (setq anchor-blocked T)
                    )
                  )
                )
              )
            )

            ;; 2. 끝점 코너 방향 간섭 확인
            (if (and p-end next-seg-real-idx (< next-seg-real-idx n-verts) (< d-from-end 2000.0) (not anchor-blocked))
              (progn
                (setq seg-next-v (nth next-seg-real-idx orig-vertices))
                (if seg-next-v
                  (progn
                    (setq adj-ang-end (angle seg-end-v seg-next-v))
                    (setq dist-to-adj-end (abs (* (sin (- (angle seg-end-v pt-base-temp) adj-ang-end)) (distance seg-end-v pt-base-temp))))

                    (setq next-wall-thick 150.0)
                    (setq next-seg-data (nth next-seg-real-idx *segment-list*))
                    (if (= (cdr (assoc 'WALL-TYPE next-seg-data)) "CIP")
                      (setq next-wall-thick (/ (atof (if (assoc 'CIP-DIA next-seg-data) (cdr (assoc 'CIP-DIA next-seg-data)) "400")) 2.0))
                      (progn
                        (setq next-timber-thick (cdr (assoc 'TIMBER-THICKNESS next-seg-data)))
                        (if (null next-timber-thick) (setq next-timber-thick 60))
                        (setq next-spec (cdr (assoc 'WALL-SPEC next-seg-data)))
                        (if (= next-spec "User-defined") (setq next-vals (cdr (assoc 'WALL-CUSTOM next-seg-data))) (setq next-vals (parse-h-spec next-spec)))
                        (if next-vals (setq next-wall-thick (+ (nth 3 next-vals) (/ next-timber-thick 2.0))))
                      )
                    )
                    (setq next-wale-spec (cdr (assoc 'WALE-SPEC next-seg-data)))
                    (if (= next-wale-spec "User-defined") (setq next-wale-vals (cdr (assoc 'WALE-CUSTOM next-seg-data))) (setq next-wale-vals (parse-h-spec next-wale-spec)))
                    (if (null next-wale-vals) (setq next-wale-vals '(300 300 10 15)))

                    (setq next-inward-norm (+ (angle seg-end-v seg-next-v) (* boundary-orient (/ pi 2.0))))
                    (setq vec-p-x2 (- (car pt-base-temp) (car seg-end-v)))
                    (setq vec-p-y2 (- (cadr pt-base-temp) (cadr seg-end-v)))
                    (setq dot-val2 (+ (* vec-p-x2 (cos next-inward-norm)) (* vec-p-y2 (sin next-inward-norm))))

                    (setq next-long-dir (angle seg-end-v seg-next-v))
                    (setq long-proj-end (+ (* vec-p-x2 (cos next-long-dir)) (* vec-p-y2 (sin next-long-dir))))

                    (if (and (> dot-val2 0)
                             (> long-proj-end (- anchor-half-width))
                             (< dist-to-adj-end (+ next-wall-thick (nth 0 next-wale-vals) anchor-half-width)))
                      (setq anchor-blocked T)
                    )
                  )
                )
              )
            )
          )
        )

        (if (and has-anchor (not anchor-blocked))
          (progn
            (create-layer-if-not-exists "_앵커(Anchor)" "6")
            (setq cos-val (cos (* max-anchor-ang (/ pi 180.0))))
            (setq proj-free (* max-anchor-free 1000.0 cos-val) proj-bond (* max-anchor-bond 1000.0 cos-val) proj-prot (* 150.0 cos-val))

            (setq pt-base (polar anchor-pt anchor-inward-dir (+ (/ d 2.0) cip-wale-h)))

            (setq cur-plan-offset 0.0)
            (foreach s-item cur-support-list
              (if (wcmatch (car s-item) "*앵커*")
                (progn
                  (setq anchor-manual-angles (if (> (length s-item) 9) (nth 9 s-item) '()))
                  (foreach m-ang anchor-manual-angles
                    (if (< (distance (list (car pt-base) (cadr pt-base)) (list (car (car m-ang)) (cadr (car m-ang)))) 50.0)
                      (setq cur-plan-offset (cdr m-ang))
                    )
                  )
                )
              )
            )
            (setq anchor-final-outward (+ anchor-earth-dir (* cur-plan-offset (/ pi 180.0))))

            (setq pt-free-end (polar pt-base anchor-final-outward proj-free))
            (setq pt-bond-end (polar pt-free-end anchor-final-outward proj-bond))

            (setq pt-h1 (polar pt-base (+ anchor-final-outward (/ pi 2.0)) 100.0) pt-h2 (polar pt-base (- anchor-final-outward (/ pi 2.0)) 100.0))
            (setq pt-h3 (polar pt-h2 (+ anchor-final-outward pi) 60.0) pt-h4 (polar pt-h1 (+ anchor-final-outward pi) 60.0))

            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 4) '(70 . 1) (list 10 (car pt-h1) (cadr pt-h1)) (list 10 (car pt-h2) (cadr pt-h2)) (list 10 (car pt-h3) (cadr pt-h3)) (list 10 (car pt-h4) (cadr pt-h4))))
            (setq pt-anchor-start (polar pt-base (+ anchor-final-outward pi) (+ 60.0 proj-prot)))

            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 2) '(70 . 0) (list 10 (car pt-anchor-start) (cadr pt-anchor-start)) (list 10 (car pt-base) (cadr pt-base))))
            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 2) '(70 . 0) (list 10 (car pt-base) (cadr pt-base)) (list 10 (car pt-free-end) (cadr pt-free-end))))
            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 2) '(70 . 0) '(43 . 400.0) (list 10 (car pt-free-end) (cadr pt-free-end)) (list 10 (car pt-bond-end) (cadr pt-bond-end))))
          )
        )
        (setq last-anchor-i i)
      )
    )

    (setq local-i (1+ local-i))
    (setq local-anchor-i (1+ local-anchor-i))

    (setq i (1+ i))
  )

  ;; ========================================================================
  ;; [STEP 3] 세그먼트별 동적 감지를 통한 독립 간격 그라우팅 작도
  ;; ========================================================================
  (setq old-offsetgap (getvar "OFFSETGAPTYPE"))
  (setvar "OFFSETGAPTYPE" 1)

  (setq cur-grouting-offset-idx (if (assoc 'CIP-GROUTING-OFFSET-IDX seg-data) (cdr (assoc 'CIP-GROUTING-OFFSET-IDX seg-data)) "0"))
  (setq cur-grouting-offset-custom (if (assoc 'CIP-GROUTING-OFFSET-CUSTOM seg-data) (cdr (assoc 'CIP-GROUTING-OFFSET-CUSTOM seg-data)) ""))
  (setq actual-grout-offset
    (if (= cur-grouting-offset-idx "1")
      (if (and cur-grouting-offset-custom (/= cur-grouting-offset-custom "")) (atof cur-grouting-offset-custom) d)
      d
    )
  )
  (setq cur-grouting-space-idx (if (assoc 'CIP-GROUTING-SPACE-IDX seg-data) (cdr (assoc 'CIP-GROUTING-SPACE-IDX seg-data)) "0"))
  (setq auto-grout-mode (and (= grouting-dia 200.0) (= cur-grouting-space-idx "4")))

  (setq vla-grout-offset-list (vl-catch-all-apply 'vla-offset (list vla-pl actual-grout-offset)))
  (if (not (vl-catch-all-error-p vla-grout-offset-list))
    (progn
      (setq vla-grout-path (vlax-safearray-get-element (vlax-variant-value vla-grout-offset-list) 0))

      (if is-closed
        (progn
          (setq grout-total-len (vlax-curve-getDistAtParam vla-grout-path (vlax-curve-getEndParam vla-grout-path)))
          (setq grout-sample-inside-count 0)
          (foreach ratio '(0.25 0.5 0.75)
            (setq grout-sample-pt (vlax-curve-getPointAtDist vla-grout-path (* grout-total-len ratio)))
            (if (and grout-sample-pt (tsp-is-point-inside grout-sample-pt orig-vertices))
              (setq grout-sample-inside-count (1+ grout-sample-inside-count))
            )
          )
          (if (>= grout-sample-inside-count 2)
            (progn
              (setq tmp-off (tsp-safe-vla-offset vla-pl (- actual-grout-offset)))
              (if tmp-off
                (progn
                  (princ "\n[경고] 그라우팅 오프셋 재생성 실패 - 기존 오프셋 경로로 대체합니다.")
                  (vla-delete vla-grout-path)
                  (setq vla-grout-path tmp-off)
                )
              )
            )
          )
        )
        (progn
          (setq grout-total-len (vlax-curve-getDistAtParam vla-grout-path (vlax-curve-getEndParam vla-grout-path)))
          (setq grout-sample-fail-count 0)
          (foreach ratio '(0.25 0.5 0.75)
            (setq pt-off (vlax-curve-getPointAtDist vla-grout-path (* grout-total-len ratio)))
            (if pt-off
              (progn
                (setq pt-orig (vlax-curve-getClosestPointTo vla-pl pt-off))
                (setq orig-param-test (vlax-curve-getParamAtPoint vla-pl pt-orig))

                (setq param-safe orig-param-test)
                (if (equal param-safe (fix param-safe) 1e-4)
                  (if (>= param-safe (vlax-curve-getEndParam vla-pl))
                    (setq param-safe (- param-safe 1e-3))
                    (setq param-safe (+ param-safe 1e-3))
                  )
                )

                (setq orig-deriv (vlax-curve-getFirstDeriv vla-pl param-safe))
                (setq orig-tangent (angle '(0 0 0) orig-deriv))

                (setq expected-offset-norm (- orig-tangent (* boundary-orient (/ pi 2.0))))
                (setq vec-expect-x (cos expected-offset-norm))
                (setq vec-expect-y (sin expected-offset-norm))

                (setq vec-actual-x (- (car pt-off) (car pt-orig)))
                (setq vec-actual-y (- (cadr pt-off) (cadr pt-orig)))

                (setq dot-prod (+ (* vec-actual-x vec-expect-x) (* vec-actual-y vec-expect-y)))

                (if (< dot-prod 0.0)
                  (setq grout-sample-fail-count (1+ grout-sample-fail-count))
                )
              )
            )
          )
          (if (>= grout-sample-fail-count 2)
            (progn
              (setq tmp-off (tsp-safe-vla-offset vla-pl (- actual-grout-offset)))
              (if tmp-off
                (progn
                  (princ "\n[경고] 그라우팅 오프셋 재생성 실패 - 기존 오프셋 경로로 대체합니다.")
                  (vla-delete vla-grout-path)
                  (setq vla-grout-path tmp-off)
                )
              )
            )
          )
        )
      )
      (setvar "OFFSETGAPTYPE" old-offsetgap)

      (setq max-grout-len (vlax-curve-getdistatparam vla-grout-path (vlax-curve-getendparam vla-grout-path)))
      (setq grout-dist 0.0)
      (create-layer-if-not-exists "_그라우팅" "5")

      (setq drawn-grout-pts '())

      (if auto-grout-mode
        (progn
          (setq cip-chain-pts cip-center-list)
          (if (and is-closed (> (length cip-chain-pts) 1)
                   (< (distance (car cip-chain-pts) (last cip-chain-pts)) 1.0))
            (setq cip-chain-pts (reverse (cdr (reverse cip-chain-pts))))
          )
          (setq cip-gap-total (if is-closed (length cip-chain-pts) (1- (length cip-chain-pts))))
          (if (< cip-gap-total 0) (setq cip-gap-total 0))
          (setq cip-gap-i 0)
          (while (< cip-gap-i cip-gap-total)
            (setq cip-gap-p1 (nth cip-gap-i cip-chain-pts))
            (setq cip-gap-p2
              (if (and is-closed (= cip-gap-i (1- (length cip-chain-pts))))
                (nth 0 cip-chain-pts)
                (nth (1+ cip-gap-i) cip-chain-pts)
              )
            )
            (if (and cip-gap-p1 cip-gap-p2 (> (distance cip-gap-p1 cip-gap-p2) 0.001))
              (progn
                (setq cip-gap-mid (list (/ (+ (car cip-gap-p1) (car cip-gap-p2)) 2.0)
                                        (/ (+ (cadr cip-gap-p1) (cadr cip-gap-p2)) 2.0)
                                        0.0
                                  )
                )
                (setq cip-gap-s (distance cip-gap-p1 cip-gap-p2))
                (setq cip-gap-ang (angle cip-gap-p1 cip-gap-p2))
                (setq cip-gap-y
                  (sqrt (max 0.0
                          (- (expt (+ (/ d 2.0) (/ grouting-dia 2.0)) 2.0)
                             (expt (/ cip-gap-s 2.0) 2.0)
                          )
                        )
                  )
                )
                (setq cip-gap-ptA (polar cip-gap-mid (+ cip-gap-ang (/ pi 2.0)) cip-gap-y))
                (setq cip-gap-ptB (polar cip-gap-mid (- cip-gap-ang (/ pi 2.0)) cip-gap-y))
                (setq cip-gap-near (vlax-curve-getClosestPointTo vla-grout-path cip-gap-ptA))
                (setq cip-gap-dA (if cip-gap-near (distance cip-gap-ptA cip-gap-near) 0.0))
                (setq cip-gap-near (vlax-curve-getClosestPointTo vla-grout-path cip-gap-ptB))
                (setq cip-gap-dB (if cip-gap-near (distance cip-gap-ptB cip-gap-near) 0.0))
                (setq cip-gap-pt (if (<= cip-gap-dA cip-gap-dB) cip-gap-ptA cip-gap-ptB))
                (tsp-draw-advanced-grouting cip-gap-pt grouting-dia cip-center-list d)
                (setq drawn-grout-pts (cons cip-gap-pt drawn-grout-pts))
              )
            )
            (setq cip-gap-i (1+ cip-gap-i))
          )
        )
        (progn
          (while (<= grout-dist max-grout-len)
            (setq grout-pt (vlax-curve-getpointatdist vla-grout-path grout-dist))

            (setq p-on-orig (vlax-curve-getClosestPointTo vla-pl grout-pt))
            (setq p-param (vlax-curve-getParamAtPoint vla-pl p-on-orig))
            (setq target-k-grout (fix p-param))
            (if (>= target-k-grout (1- (length chain-pts))) (setq target-k-grout (- (length chain-pts) 2)))

            (setq target-index-grout (nth target-k-grout chain-seg-indices))
            (if (null target-index-grout) (setq target-index-grout seg-idx))

            (setq cur-seg-data (nth target-index-grout *segment-list*))

            (setq cur-grouting-space-idx (if (assoc 'CIP-GROUTING-SPACE-IDX cur-seg-data) (cdr (assoc 'CIP-GROUTING-SPACE-IDX cur-seg-data)) "0"))
            (setq cur-grouting-custom (if (assoc 'CIP-GROUTING-SPACE-CUSTOM cur-seg-data) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM cur-seg-data)) ""))
            (setq cur-grouting-dia-idx (if (assoc 'CIP-GROUTING-IDX cur-seg-data) (cdr (assoc 'CIP-GROUTING-IDX cur-seg-data)) "0"))
            (setq cur-grouting-dia (if (= cur-grouting-dia-idx "1") 600.0 200.0))

            (setq grout-space-val
              (cond
                ((= cur-grouting-space-idx "0") 200.0)
                ((= cur-grouting-space-idx "1") 400.0)
                ((= cur-grouting-space-idx "2") 600.0)
                (t (if (and cur-grouting-custom (/= cur-grouting-custom "")) (atof cur-grouting-custom) 200.0))
              )
            )

            (setq skip-grout (= cur-grouting-dia-idx "2"))
            (if (and (not skip-grout) full-loop drawn-grout-pts)
              (progn
                (if (< (distance grout-pt (last drawn-grout-pts)) (* grout-space-val 0.15))
                  (setq skip-grout T)
                )
              )
            )

            (if (not skip-grout)
              (progn
                (tsp-draw-advanced-grouting grout-pt cur-grouting-dia cip-center-list d)
                (setq drawn-grout-pts (cons grout-pt drawn-grout-pts))
              )
            )

            (setq rem-grout-len (- max-grout-len grout-dist))
            (if full-loop
              (cond
                ((<= rem-grout-len (* grout-space-val 1.05))
                 (setq grout-dist (+ max-grout-len 10.0)))
                ((<= rem-grout-len (* grout-space-val 2.0))
                 (setq grout-dist (+ grout-dist (/ rem-grout-len 2.0))))
                (t
                 (setq grout-dist (+ grout-dist grout-space-val)))
              )
              (cond
                ((<= rem-grout-len 1.0)
                 (setq grout-dist (+ max-grout-len 10.0)))
                ((<= rem-grout-len (* grout-space-val 1.05))
                 (setq grout-dist max-grout-len))
                ((<= rem-grout-len (* grout-space-val 2.0))
                 (setq grout-dist (+ grout-dist (/ rem-grout-len 2.0))))
                (t
                 (setq grout-dist (+ grout-dist grout-space-val)))
              )
            )
          )
        )
      )
      (vla-delete vla-grout-path)
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-draw-cip-plan-per-segment : 세그먼트별 연속 CIP 작도 및 브래킷 통합
;;;;==========================================================================
(defun tsp-draw-cip-plan-per-segment (boundary-ent boundary-orient / doc mspace br-layer br-color is-closed orig-vertices n-verts max-segs visited-segs seg-idx seg-data seg-wall-type chain-pts chain-seg-indices j stop-chain next-data next-type end-pt save-dia save-mode save-overlap save-equal-divide save-interval save-anchor-n save-hpile-idx save-grouting grouting-dia d wall-dist cip-ctc hpile-vals h b hpile-block interval is-full-chain geo-path-pts temp-pl vla-pl in-norm earth-dir old-offsetgap vla-offset-list vla-path pt-orig pt-off vec-expect-x vec-expect-y vec-actual-x vec-actual-y dot-prod tmp-off layout-pack cip-layout-items hpile-flags corner-pts max-path-len full-loop total-counted-in-list v-idx)

  ;; COM 객체용 공간 및 레이어 설정 (브래킷용)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (if (= (getvar "CVPORT") 1)
    (setq mspace (vla-get-paperspace doc))
    (setq mspace (vla-get-modelspace doc))
  )
  (setq br-layer "_브래킷(bracket)")
  (setq br-color 8)
  (create-layer-if-not-exists br-layer (itoa br-color))

  (setq is-closed (is-closed-polyline boundary-ent))
  (setq orig-vertices (mapcar '(lambda (s) (cdr (assoc 'V-START s))) *segment-list*))
  (setq *tsp-actual-drawn-hpiles* 0)
  (if (not is-closed)
    (setq orig-vertices (append orig-vertices (list (cdr (assoc 'V-END (last *segment-list*))))))
  )
  (setq n-verts (length orig-vertices))
  (setq max-segs (if is-closed n-verts (1- n-verts)))

  (setq visited-segs '())
  (setq seg-idx 0)
  (while (< seg-idx max-segs)
    (setq seg-data (nth seg-idx *segment-list*))
    (setq seg-wall-type (cdr (assoc 'WALL-TYPE seg-data)))

    (if (and (= seg-wall-type "CIP") (cdr (assoc 'IS-DEFINED seg-data)) (not (member seg-idx visited-segs)))
      (progn
        ;; 1. 연속된 CIP 세그먼트 찾기 (원형 체인 형성 및 실제 인덱스 보존)
        (setq chain-pts (list (nth seg-idx orig-vertices)))
        (setq chain-seg-indices (list seg-idx))
        (setq visited-segs (cons seg-idx visited-segs))
        (setq j (if is-closed (rem (1+ seg-idx) max-segs) (1+ seg-idx)))
        (setq stop-chain nil)

        (while (and (not stop-chain) (/= j seg-idx) (if is-closed T (< j max-segs)) (not (member j visited-segs)))
          (setq next-data (nth j *segment-list*))
          (setq next-type (cdr (assoc 'WALL-TYPE next-data)))
          (if (and (= next-type "CIP") (cdr (assoc 'IS-DEFINED next-data)))
            (progn
              (setq chain-pts (append chain-pts (list (nth j orig-vertices))))
              (setq chain-seg-indices (append chain-seg-indices (list j)))
              (setq visited-segs (cons j visited-segs))
              (setq j (if is-closed (rem (1+ j) max-segs) (1+ j)))
            )
            (setq stop-chain T)
          )
        )
        ;; 체인의 마지막 끝점 안전하게 추가
        (setq end-pt (if (< j n-verts) (nth j orig-vertices) (nth 0 orig-vertices)))
        (setq chain-pts (append chain-pts (list end-pt)))
        (setq chain-seg-indices (append chain-seg-indices (list j)))

        ;; 2. 세그먼트 파라미터 로드
        (setq save-dia *tsp-cip-dia*
              save-mode *tsp-cip-mode-idx*
              save-overlap *tsp-cip-overlap*
              save-equal-divide *tsp-cip-equal-divide*
              save-interval *tsp-cip-interval-idx*
              save-anchor-n *tsp-cip-anchor-n*
              save-hpile-idx *tsp-cip-hpile-idx*
              save-grouting *tsp-cip-grouting-idx*)
        (setq *tsp-cip-dia* (cdr (assoc 'CIP-DIA seg-data)))
        (setq *tsp-cip-mode-idx* (cdr (assoc 'CIP-MODE-IDX seg-data)))
        (setq *tsp-cip-overlap* (cdr (assoc 'CIP-OVERLAP seg-data)))
        (setq *tsp-cip-equal-divide* (if (assoc 'CIP-EQUAL-DIVIDE seg-data) (cdr (assoc 'CIP-EQUAL-DIVIDE seg-data)) "0"))
        (setq *tsp-cip-interval-idx* (cdr (assoc 'CIP-INTERVAL-IDX seg-data)))
        (setq *tsp-cip-anchor-n*
          (if (assoc 'CIP-ANCHOR-N seg-data)
            (cdr (assoc 'CIP-ANCHOR-N seg-data))
            (itoa (1+ (atoi (if *tsp-cip-interval-idx* *tsp-cip-interval-idx* "2"))))
          )
        )
        (setq *tsp-cip-hpile-idx* (cdr (assoc 'CIP-HPILE-IDX seg-data)))
        (setq *tsp-cip-grouting-idx* (cdr (assoc 'CIP-GROUTING-IDX seg-data)))
        (setq grouting-dia (if (= *tsp-cip-grouting-idx* "1") 600.0 200.0))

        (setq d (atof *tsp-cip-dia*))
        (setq wall-dist (/ d 2.0))
        (setq cip-ctc (if (= *tsp-cip-mode-idx* "0") d (- d (atof *tsp-cip-overlap*))))
        (setq hpile-vals (parse-h-spec (nth (atoi *tsp-cip-hpile-idx*) *tsp-std-wall-list*)))
        (setq h (nth 0 hpile-vals) b (nth 1 hpile-vals))
        (setq hpile-block (create-hpile-section-block h b (nth 2 hpile-vals) (nth 3 hpile-vals)))
        (setq interval (cond ((= *tsp-cip-interval-idx* "1") 2) ((= *tsp-cip-interval-idx* "2") 3) (t 1)))

        ;; 3. 임시 연속 폴리선 생성 및 오프셋
        (setq is-full-chain (and is-closed (> (length chain-pts) 1) (< (distance (car chain-pts) (last chain-pts)) 0.001)))
        (if is-full-chain
          (setq chain-pts (reverse (cdr (reverse chain-pts))))
        )
        (setq geo-path-pts (tsp-geo-augment-chain boundary-ent chain-pts))
        (if (or (null geo-path-pts) (< (length geo-path-pts) 2)) (setq geo-path-pts chain-pts))
        (setq temp-pl (entmakex (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                              (cons 90 (length geo-path-pts)) (cons 70 (if is-full-chain 1 0)))
                                        (mapcar '(lambda (p) (list 10 (car p) (cadr p))) geo-path-pts))))
        (setq vla-pl (if temp-pl (vl-catch-all-apply 'vlax-ename->vla-object (list temp-pl)) nil))
        (if (vl-catch-all-error-p vla-pl) (setq vla-pl nil))

        (setq in-norm (+ (angle (nth 0 chain-pts) (nth 1 chain-pts)) (* boundary-orient (/ pi 2.0))))
        (setq earth-dir (+ in-norm pi))

        (setq old-offsetgap (getvar "OFFSETGAPTYPE"))
        (setvar "OFFSETGAPTYPE" 1)

        (setq vla-offset-list (vl-catch-all-apply 'vla-offset (list vla-pl (- wall-dist))))
        (if (not (vl-catch-all-error-p vla-offset-list))
          (progn
            (setq vla-path (vlax-safearray-get-element (vlax-variant-value vla-offset-list) 0))

            ;; [방향 검증] 내적(Dot Product)을 이용한 안팎 판정 로직
            (setq pt-orig (vlax-curve-getPointAtParam vla-pl 0.5))
            (setq pt-off (vlax-curve-getClosestPointTo vla-path pt-orig))
            (if (and pt-orig pt-off)
              (progn
                (setq vec-expect-x (cos earth-dir))
                (setq vec-expect-y (sin earth-dir))
                (setq vec-actual-x (- (car pt-off) (car pt-orig)))
                (setq vec-actual-y (- (cadr pt-off) (cadr pt-orig)))
                (setq dot-prod (+ (* vec-actual-x vec-expect-x) (* vec-actual-y vec-expect-y)))

                (if (< dot-prod 0.0)
                  (progn
                    (setq tmp-off (tsp-safe-vla-offset vla-pl wall-dist))
                    (if tmp-off
                      (progn
                        (princ "\n[경고] CIP 오프셋 재생성 실패 - 기존 오프셋 경로로 대체합니다.")
                        (vla-delete vla-path)
                        (setq vla-path tmp-off)
                      )
                    )
                  )
                )
              )
            )

            (setvar "OFFSETGAPTYPE" old-offsetgap)

            ;; 4-1. 코너 선탐색, 겹침 분배 및 H-Pile 시뮬레이션 (TSP_4I_Plan.lsp)
            (setq layout-pack
              (tsp-cip-calc-chain-layout
                vla-pl vla-path chain-pts chain-seg-indices is-full-chain is-closed max-segs
                boundary-orient d wall-dist cip-ctc hpile-vals h b interval seg-idx
              )
            )
            (setq cip-layout-items (nth 0 layout-pack)
                  hpile-flags      (nth 1 layout-pack)
                  corner-pts       (nth 2 layout-pack)
                  max-path-len     (nth 3 layout-pack)
                  full-loop        (nth 4 layout-pack))

            ;; 4-2. CIP 본체, H-Pile, 브래킷, 앵커 및 그라우팅 작도
            (tsp-draw-cip-elements-and-grout
              vla-pl vla-path cip-layout-items hpile-flags corner-pts chain-pts chain-seg-indices
              orig-vertices n-verts is-closed full-loop max-path-len boundary-orient in-norm
              d cip-ctc h b hpile-block seg-idx seg-data grouting-dia mspace br-layer br-color
            )

            (vla-delete vla-path)
          )
        )
        (if temp-pl (entdel temp-pl))

        ;; 5. 복구 및 점프
        (setq *tsp-cip-dia* save-dia
              *tsp-cip-mode-idx* save-mode
              *tsp-cip-overlap* save-overlap
              *tsp-cip-equal-divide* save-equal-divide
              *tsp-cip-interval-idx* save-interval
              *tsp-cip-hpile-idx* save-hpile-idx
              *tsp-cip-grouting-idx* save-grouting)
      )
    )
    (setq seg-idx (1+ seg-idx))
  )

  ;; 검증(Validation) 단계
  (setq total-counted-in-list 0 v-idx 0)
  (while (< v-idx (length *segment-list*))
    (if (= (cdr (assoc 'WALL-TYPE (nth v-idx *segment-list*))) "CIP")
      (setq total-counted-in-list (+ total-counted-in-list (nth v-idx *tsp-segment-pile-counts*)))
    )
    (setq v-idx (1+ v-idx))
  )

  (if (= total-counted-in-list *tsp-actual-drawn-hpiles*)
    (princ (strcat "\n[시스템 검증 성공] C.I.P 누적 캡슐 총합(" (itoa total-counted-in-list) ")과 실제 도면 생성 개수(" (itoa *tsp-actual-drawn-hpiles*) ")가 완벽히 일치합니다."))
    (princ (strcat "\n[시스템 검증 경고] C.I.P 개수 불일치 발생! 누적 총합: " (itoa total-counted-in-list) " / 실제 생성 개수: " (itoa *tsp-actual-drawn-hpiles*)))
  )
)

;;;;==========================================================================
;;;; defun tsp-find-next-dist-by-chord : 현재 점에서 목표 현의 길이(직선 거리)를 만족하는 곡선상의 다음 누적 거리를 이분법으로 탐색
;;;;==========================================================================
(defun tsp-find-next-dist-by-chord (curve-obj current-dist target-chord max-dist /
                                    pt-current dist-low dist-high dist-mid
                                    pt-mid current-chord tolerance iter max-iter pt-high)
  (setq pt-current (vlax-curve-getPointAtDist curve-obj current-dist))
  (setq dist-low current-dist)

  ;; 1차 검증: 남은 '경로 거리(Path)' 자체가 목표 간격보다 짧으면 탐색 불가 (폐합선 오류 방지)
  (if (< (- max-dist current-dist) target-chord)
    nil
    (progn
      ;; 상한선은 목표 직선거리의 1.5배 또는 전체 길이 중 작은 값으로 설정
      (setq dist-high (min (+ current-dist (* target-chord 1.5)) max-dist))

      ;; 상한선을 확장하는 안전장치 (곡률이 매우 심할 경우 대비)
      (while (and (< (distance pt-current (vlax-curve-getPointAtDist curve-obj dist-high)) target-chord)
                  (< dist-high max-dist))
        (setq dist-high (min (+ dist-high target-chord) max-dist))
      )

      (setq pt-high (vlax-curve-getPointAtDist curve-obj dist-high))

      ;; 최종 검증: max-dist까지 경로를 따라갔는데도 직선거리가 목표치보다 짧다면 도달 불가
      (if (and (= dist-high max-dist) (< (distance pt-current pt-high) target-chord))
        nil
        (progn
          (setq tolerance 1e-8)
          (setq iter 0 max-iter 150)
          (setq dist-mid dist-low)

          ;; 이분법 정밀 탐색
          (while (and (< iter max-iter) (< tolerance (- dist-high dist-low)))
            (setq dist-mid (/ (+ dist-low dist-high) 2.0))
            (setq pt-mid (vlax-curve-getPointAtDist curve-obj dist-mid))
            (setq current-chord (distance pt-current pt-mid))

            (if (< current-chord target-chord)
              (setq dist-low dist-mid)
              (setq dist-high dist-mid)
            )
            (setq iter (1+ iter))
          )
          dist-mid
        )
      )
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-make-segment-guideline : 세그먼트 구간 보조선 생성 (직접 좌표 계산으로 Offset 오류 원천 차단)
;;;;==========================================================================
(defun tsp-make-segment-guideline (pt1 pt2 dia boundary-orient / d seg-angle out-norm new-pt1 new-pt2 guide-ent)
  (setq d (/ dia 2.0))
  (setq seg-angle (angle pt1 pt2))
  (setq out-norm (+ seg-angle (* boundary-orient (/ pi 2.0))))

  ;; H-Pile(토류판)과 똑같이 바깥쪽을 향하도록 마이너스(-d) 방향으로 점 이동
  (setq new-pt1 (polar pt1 out-norm (- d)))
  (setq new-pt2 (polar pt2 out-norm (- d)))

  ;; 캐드 내장 OFFSET 명령어의 방향 플립(Flip) 버그에 의한 오검증 문제를 원천 차단하기 위해
  ;; 계산된 좌표로 보조 폴리선을 수학적으로 직접 강제 작도합니다.
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                 '(8 . "0") '(62 . 256) '(90 . 2) '(70 . 0)
                 (list 10 (car new-pt1) (cadr new-pt1))
                 (list 10 (car new-pt2) (cadr new-pt2))))
  (setq guide-ent (entlast))
  guide-ent
)

;;;;==========================================================================
;;;; defun tsp-draw-cip-plan : C.I.P 평면도 작도 메인 제어 흐름
;;;;==========================================================================
(defun tsp-draw-cip-plan (boundary-ent boundary-orient / backup-segment-list backup-pile-counts backup-actual-hpiles backup-wall-type err)
  (setq backup-segment-list *segment-list*)
  (setq backup-pile-counts *tsp-segment-pile-counts*)
  (setq backup-actual-hpiles (if (boundp '*tsp-actual-drawn-hpiles*) *tsp-actual-drawn-hpiles* nil))
  (setq backup-wall-type *tsp-wall-type*)

  (setq err
    (vl-catch-all-apply
      '(lambda ()
         ;; 전역 폴백도 내부적으로는 세그먼트 기반으로 처리
         (init-segment-data boundary-ent boundary-orient nil)

         (if *segment-list*
           (progn
             (setq *tsp-wall-type* "CIP")
             (tsp-draw-cip-plan-per-segment boundary-ent boundary-orient)
           )
           (princ "\n[에러] 세그먼트 데이터 생성에 실패했습니다.")
         )
      )
      '()
    )
  )

  ;; 호출 전 상태 복원
  (setq *segment-list* backup-segment-list)
  (setq *tsp-segment-pile-counts* backup-pile-counts)
  (setq *tsp-actual-drawn-hpiles* backup-actual-hpiles)
  (setq *tsp-wall-type* backup-wall-type)

  (if (vl-catch-all-error-p err)
    (princ (strcat "\n[에러] 전역 CIP 폴백 통합 작도 중 오류: " (vl-catch-all-error-message err)))
  )

  (princ)
)
