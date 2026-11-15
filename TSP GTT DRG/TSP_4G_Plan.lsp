;;; ==========================================================================
;;; 파일명: TSP_4G_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part G)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [create-hpile-set-on-boundary, tsp-ceil, tsp-create-cip-guideline, tsp-calc-cip-layout, tsp-draw-cip-elements 외 3개]
;;; 포함 함수: create-hpile-set-on-boundary, tsp-ceil, tsp-create-cip-guideline, tsp-calc-cip-layout, tsp-draw-cip-elements, draw-bracket-poly, tsp-acos, tsp-normalize-angle
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;; defun create-hpile-set-on-boundary : 평면도 생성 (H-Pile / C.I.P 분기 처리 적용)
;;;;==========================================================================
(defun create-hpile-set-on-boundary (boundary-ent boundary-orient / last-ent vertices
                                      seg-data seg-wall-type has-cip has-hpile i
                                      vla-bnd total-len s-item s-manual-pts s-sec-str s-jack-type
                                      remapped-pts p d new-pt s-pt1 s-pt2 draw-result)

  ;; 1. 기존 그룹 정리 (번호표 포함 모든 객체 삭제)
  (tsp-clear-drawing-group boundary-ent)
  ;; 2. 마지막 엔티티 추적 시작 (기존 그룹 삭제 직후의 안전한 기준점)
  (setq last-ent (entlast))
  (setq *tsp-last-group-start-ent* last-ent)

  ;; 모든 세그먼트의 말뚝 개수를 누적할 전역 리스트를 0으로 사전 초기화
  (setq *tsp-segment-pile-counts* '())
  (setq i 0)
  (while (< i (length *segment-list*))
    (setq *tsp-segment-pile-counts* (cons 0 *tsp-segment-pile-counts*))
    (setq i (1+ i))
  )
  (setq *tsp-segment-pile-counts* (reverse *tsp-segment-pile-counts*))

  ;; 데이터 캐싱을 위한 H-Pile 좌표 리스트 초기화
  (setq *tsp-hpile-points* '())

  ;; 3. 시스템 필수 요소 선제적 초기화 (레이어, 블록, 선종류, 확장데이터 APPID)
  ;; [디버그] 블록(Block) 생성이 실패하는 것을 막기 위해, 블록이 종속될 레이어(Layer)들을 최우선으로 강제 생성합니다.
  (foreach lay-info '(("_mpoint" 2) ("_버팀보(Strut)" 4) ("_사보강재(Corner_Strut)" 3) ("_보강재(Brace)" 7) ("_화타쐐기(Wedge)" 2) ("_스크류잭" 2) ("_유압잭" 2) ("_중간말뚝(Post pile)" 3) ("_까치발(Corbel)" 3))
    (if (not (tblsearch "LAYER" (car lay-info)))
      (vl-catch-all-apply 'entmake (list (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 (car lay-info)) '(70 . 0) (cons 62 (cadr lay-info)))))
    )
  )
  (if (boundp 'create-strut-jack-block) (vl-catch-all-apply 'create-strut-jack-block))
  (if (boundp 'create-strut-screw-jack-block) (vl-catch-all-apply 'create-strut-screw-jack-block))
  (if (not (tblsearch "APPID" "TSP_STRUT_SPEC")) (regapp "TSP_STRUT_SPEC"))
  (if (not (tblsearch "APPID" "TSP_LISP_POST")) (regapp "TSP_LISP_POST"))
  (if (not (tblsearch "APPID" "TSP_LISP_BRACE")) (regapp "TSP_LISP_BRACE"))
  (if (not (tblsearch "LTYPE" "DASHED")) (vl-catch-all-apply 'vla-load (list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) "DASHED" "acad.lin")))

  ;; 4. 안전망(vl-catch-all-apply) 구축: 내부 로직에서 에러가 발생해도 튕기지 않도록 보호
  (setq draw-result
    (vl-catch-all-apply
      '(lambda ()
         ;; 4-1. SUB-ZONES(구간 분할)가 있는 세그먼트를 벽체 스팬별로 전개(중첩 구간은 강성이 큰 벽체 우선: CIP > HPILE, 동일 공법 시 H-Pile H가 큰 것 우선)
         (setq orig-segs-for-wall *segment-list*)
         (setq orig-counts-for-wall *tsp-segment-pile-counts*)
         (setq expanded-wall-pack (tsp-expand-segments-for-wall-plan *segment-list*))
         (setq *segment-list* (car expanded-wall-pack))
         (setq wall-parent-map (cadr expanded-wall-pack))
         ;; 4-1-1. [경계선 추종] 세그먼트를 쪼개지 않는다.
         ;;        설계 세그먼트(삭제된 모서리 무시) 단위로 말뚝 간격/개수를 산정한 뒤,
         ;;        좌표만 경계선 실제 형상으로 투영한다. (분할 시 코너말뚝 중복 생성 문제 방지)
         (setq *tsp-segment-pile-counts* (mapcar '(lambda (x) 0) *segment-list*))

         (setq has-cip nil  has-hpile nil)
         (if *segment-list*
           (foreach seg-data *segment-list*
             (if (cdr (assoc 'IS-DEFINED seg-data))
               (progn
                 (setq seg-wall-type (cdr (assoc 'WALL-TYPE seg-data)))
                 (if (= seg-wall-type "CIP")
                   (setq has-cip T)
                   (setq has-hpile T)
                 )
               )
             )
           )
         )
         ;; 4-2. H-Pile+토류판 세그먼트 작도 (배면 차수그라우팅 포함)
         (if has-hpile
           (progn
             (princ "\n[작도] H-Pile 및 토류판 생성 중...")
             (place-hpile-timber-along-boundary boundary-ent boundary-orient)
             (tsp-draw-hpile-grouting-per-segment boundary-ent boundary-orient)
           )
         )
         ;; 4-3. CIP 세그먼트 작도
         (if has-cip
           (progn
             (princ "\n[작도] C.I.P(현장타설말뚝) 배열 생성 중...")
             (tsp-draw-cip-plan-per-segment boundary-ent boundary-orient)
           )
         )
         ;; 4-3-1. 분할 작도된 말뚝 개수를 원래 부모 세그먼트 인덱스로 합산 후 *segment-list* 복원
         (setq merged-pile-counts (mapcar '(lambda (x) 0) orig-segs-for-wall))
         (setq k-exp 0)
         (while (< k-exp (length *tsp-segment-pile-counts*))
           (setq p-idx (nth k-exp wall-parent-map))
           (setq cur-cnt (nth k-exp *tsp-segment-pile-counts*))
           (if (and p-idx cur-cnt (< p-idx (length merged-pile-counts)))
             (setq merged-pile-counts (update-list-item merged-pile-counts p-idx (+ (nth p-idx merged-pile-counts) cur-cnt)))
           )
           (setq k-exp (1+ k-exp))
         )
         (setq *tsp-segment-pile-counts* merged-pile-counts)
         (setq *segment-list* orig-segs-for-wall)
         ;; 4-4. 전역값 폴백 (안전장치)
         (if (and (not has-cip) (not has-hpile))
           (progn
             (if (= *tsp-wall-type* "CIP")
               (progn
                 (princ "\n[작도] C.I.P(현장타설말뚝) 배열 생성 중... (전역 폴백)")
                 (tsp-draw-cip-plan boundary-ent boundary-orient)
               )
               (progn
                 (princ "\n[작도] H-Pile 및 토류판 생성 중... (전역 폴백)")
                 (place-hpile-timber-along-boundary boundary-ent boundary-orient)
                 (tsp-draw-hpile-grouting-per-segment boundary-ent boundary-orient)
               )
             )
           )
         )

                           ;; 5. 사보강재 및 띠장(Wale) 선행 작도
         ;;    (버팀보 재작도 전에 실제 _띠장(wale)을 먼저 생성해야 버팀보가 실제 띠장 내측선에 100% 정합됨)
         (if (not (boundp '*tsp-geo-follow-brace-wale*)) (setq *tsp-geo-follow-brace-wale* T))
         (if *tsp-geo-follow-brace-wale*
           (progn
             (setq geo-brace-pack (tsp-expand-segments-by-geometry *segment-list* boundary-ent))
             (if (car geo-brace-pack) (setq *segment-list* (car geo-brace-pack)))
           )
         )

         (princ "\n[작도] 사보강재(Corner Brace) 생성 중...")
         (tsp-draw-all-corner-braces boundary-ent boundary-orient)

         (princ "\n[작도] 띠장(Wale) 생성 중...")
         (create-wale-offsets boundary-ent boundary-orient)

         ;; 버팀보 및 번호표 처리는 설계 세그먼트 리스트 구조 기준이어야 하므로 원복
         (if *tsp-geo-follow-brace-wale* (setq *segment-list* orig-segs-for-wall))

         ;; 6. 버팀보(Strut) 평면 재작도 (실제 국부 띠장 법선 기준)
         (if *segment-list*
           (progn
             (princ "\n[버팀보 재작도] 데이터 분석 및 작도 시작...")
             (setq vla-bnd (vlax-ename->vla-object boundary-ent))
             (setq total-len (vlax-curve-getdistatparam vla-bnd (vlax-curve-getendparam vla-bnd)))

             (setq poly-verts (tsp-build-poly-verts boundary-ent))

             (foreach seg-data *segment-list*
               (foreach s-item (cdr (assoc 'SUPPORT-LIST seg-data))
                 (if (wcmatch (car s-item) "버팀보*")
                   (progn
                     (princ (strcat "\n[버팀보 확인] 이름: " (nth 1 s-item) " 데이터 로드 중..."))
                     (setq s-shape      (if (> (length s-item) 2) (nth 2 s-item) "H형강"))
                     (setq s-sec-str    (if (> (length s-item) 6) (nth 6 s-item) ""))
                     (setq s-jack-type  (if (> (length s-item) 7) (nth 7 s-item) ""))
                     (setq s-manual-pts (if (> (length s-item) 8) (nth 8 s-item) nil))

                     (if s-manual-pts
                       (princ (strcat "\n  -> 수집된 점 데이터 개수: " (itoa (length s-manual-pts))))
                       (princ "\n  -> 수집된 점 데이터가 없습니다 (nil).")
                     )

                                          (if (and s-manual-pts (>= (length s-manual-pts) 1))
                       (progn
                         (setq s-brace-opt (if (> (length s-item) 11) (nth 11 s-item) nil))
                         (if (= (type s-brace-opt) 'STR) (setq s-brace-opt (read s-brace-opt)))
                         (setq angle-offset-deg (atof (tsp-brace-opt-get s-brace-opt 14 "0.0")))
                         (setq s-ctc-mm (tsp-ctc-mm (if (> (length s-item) 10) (nth 10 s-item) nil)))
                         ;; [방식 A & 구버전 CSV 호환] 반올림된 CSV 좌표나 절점 통과 구간 오차를 직각 C.T.C 간격으로 정밀 복원
                         (if (boundp 'tsp-refine-strut-manual-pts)
                           (setq s-manual-pts (tsp-refine-strut-manual-pts s-manual-pts s-ctc-mm seg-data boundary-ent poly-verts angle-offset-deg))
                         )
                         (setq remapped-pts '())
                         (foreach p s-manual-pts
                           ;; 소속 세그먼트(seg-data)의 실제 국부 띠장 법선 역투영으로 경계선 기준점 산출
                           (setq new-pt (tsp-project-pick-to-seg-boundary p seg-data boundary-ent poly-verts angle-offset-deg))
                           (setq remapped-pts (cons new-pt remapped-pts))
                         )
                         (setq remapped-pts (reverse remapped-pts))
                         (princ (strcat "\n  -> 경계선 좌표 매핑 성공. 총 " (itoa (length remapped-pts)) "개의 기준점에서 레이저 투사 시작."))

                         (setq draw-count 0)
                         ;; [1H 확장 & 경사 버팀보 그룹 기준] 첫 버팀보 기준점/기준길이 및 C.T.C 초기화
                         (setq *tsp-1h-jack-base* nil)
                         (setq *tsp-group-ref-len* nil)
                         (setq *tsp-1h-jack-ctc* (tsp-ctc-mm (if (> (length s-item) 10) (nth 10 s-item) nil)))
                         ;; 현재 버팀보가 속한 세그먼트(seg-data) 및 인덱스를 직접 사용하여 이웃 세그먼트 오판 방지
                         (setq best-seg1 seg-data)
                         (setq seg-idx1 (vl-position seg-data *segment-list*))
                         (if (null seg-idx1) (setq seg-idx1 0))
                         (foreach bp remapped-pts

                           (setq p-base-flat (list (car bp) (cadr bp) 0.0))
                           ;; [핵심 수정] 설계 현(best-seg1)이 아닌 실제 국부 띠장 법선(tsp-get-local-wale-seg-info)을 사용하여
                           ;; 직접 작도 시와 [확인] 후 재작도 시의 시작점(pt1-draw) 및 발사각(ray-ang)을 100% 일치시킴
                           (setq seg1-info (tsp-get-local-wale-seg-info p-base-flat best-seg1 boundary-ent poly-verts))
                           (setq norm-ang (nth 3 seg1-info))
                           (setq offset1 (nth 4 seg1-info))
                           (setq pt1-draw (tsp-closest-pt-on-seg-2d (polar p-base-flat norm-ang offset1) (nth 5 seg1-info) (nth 6 seg1-info)))
                           (setq ray-ang (+ norm-ang (* angle-offset-deg (/ pi 180.0))))
                           (setq p-ray1 (polar pt1-draw ray-ang 1e6))

                           (setq pts '() i 0)
                           (foreach seg *segment-list*
                             (setq sp1 (trans (cdr (assoc 'V-START seg)) boundary-ent 0))
                             (setq sp2 (trans (cdr (assoc 'V-END seg)) boundary-ent 0))
                             (setq sp1-flat (list (car sp1) (cadr sp1) 0.0))
                             (setq sp2-flat (list (car sp2) (cadr sp2) 0.0))

                             (setq int-pt1 (inters pt1-draw p-ray1 sp1-flat sp2-flat T))
                             (if (and int-pt1 (/= i seg-idx1) (> (distance pt1-draw int-pt1) 10.0))
                               (setq pts (cons (list int-pt1 i ray-ang) pts))
                             )
                             (setq i (1+ i))
                           )
                           (setq pts (reverse pts))

                           (setq valid-pts '())
                           (foreach pt-data pts
                             (setq int-pt (car pt-data))
                             (setq mid-pt (list (/ (+ (car pt1-draw) (car int-pt)) 2.0) (/ (+ (cadr pt1-draw) (cadr int-pt)) 2.0) 0.0))
                             (if (tsp-is-point-inside mid-pt poly-verts)
                               (setq valid-pts (cons pt-data valid-pts))
                             )
                           )
                           (setq valid-pts (reverse valid-pts))

                           (if valid-pts
                             (progn
                               (setq valid-pts (vl-sort valid-pts '(lambda (a b) (< (distance pt1-draw (car a)) (distance pt1-draw (car b))))))
                               (setq p-opp (car (car valid-pts)))
                               (setq seg-idx2 (cadr (car valid-pts)))
                               (setq seg2-info (tsp-get-seg-inward-normal (nth seg-idx2 *segment-list*) boundary-ent poly-verts))
                               (setq offset2 (nth 4 seg2-info))
                               (setq w2-p1 (nth 5 seg2-info) w2-p2 (nth 6 seg2-info))
                               (setq pt2-draw (inters pt1-draw p-ray1 w2-p1 w2-p2 nil))
                               (if (null pt2-draw) (setq pt2-draw (polar p-opp (nth 3 seg2-info) offset2)))

                               ;; [띠장 정합] 실제 작도된 띠장 형상 기준 양단 접합점 보정
                               (setq wale-fix (tsp-wale-fix-ends pt1-draw ray-ang boundary-ent offset1 offset2 poly-verts))
                               (if wale-fix
                                 (progn
                                   (setq pt1-draw (car wale-fix) pt2-draw (cadr wale-fix))
                                   (if (caddr wale-fix) (setq p-opp (caddr wale-fix)))
                                 )
                               )

                               (if (> (distance p-base-flat p-opp) (+ offset1 offset2))
                                 (progn
                                   (if (wcmatch s-shape "H형강")
                                     (progn
                                       ;; 1. 개수를 파악하여 2H 맞버팀대 여부(is-double) 결정
                                       (setq s-count (if (> (length s-item) 4) (nth 4 s-item) 1))
                                       (if (= (type s-count) 'STR) (setq s-count (atoi s-count))) ;; CSV 텍스트 오염 완벽 방어
                                       (setq is-double (if (>= s-count 2) T nil))
                                       ;; 3. 현재 작도 기준점이 속한 세그먼트의 고유 ID 추출
                                       (setq cur-seg-id (cdr (assoc 'ID best-seg1)))
                                       ;; _mpoint 레이어 POINT 객체 생성
                                       (if (boundp 'create-layer-if-not-exists) (create-layer-if-not-exists "_mpoint" "2"))
                                       (entmake (list '(0 . "POINT") '(100 . "AcDbEntity") '(100 . "AcDbPoint") (cons 8 "_mpoint") (cons 62 2) (list 10 (car pt1-draw) (cadr pt1-draw) 0.0)))
                                       (entmake (list '(0 . "POINT") '(100 . "AcDbEntity") '(100 . "AcDbPoint") (cons 8 "_mpoint") (cons 62 2) (list 10 (car pt2-draw) (cadr pt2-draw) 0.0)))
                                       ;; 7개의 인수를 모두 전달하여 버팀보 및 부속물 통합 작도
                                       (if (null *tsp-1h-jack-base*)
                                         (progn
                                           (setq *tsp-1h-jack-base* pt1-draw)
                                           (setq *tsp-group-ref-len* (distance pt1-draw pt2-draw))
                                         )
                                       )
                                       (tsp-draw-manual-strut pt1-draw pt2-draw s-sec-str s-jack-type is-double s-brace-opt cur-seg-id)
                                       (setq draw-count (1+ draw-count))
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         ) ;; foreach bp end
                         (princ (strcat "\n  -> [최종 완료] 도면에 작도된 버팀보 묶음 개수: " (itoa draw-count)))
                       )
                     )
                   )
                 )
               )
             )
           )
         )

         (if *segment-list*
           (progn
             (setq vertices (mapcar '(lambda (item) (cdr (assoc 'V-START item))) *segment-list*))
             (if (not (is-closed-polyline boundary-ent))
               (setq vertices (append vertices (list (cdr (assoc 'V-END (last *segment-list*))))))
             )
             (draw-segment-numbers boundary-ent boundary-orient vertices)
             (draw-corner-numbers boundary-ent boundary-orient vertices)
           )
         )
      )
    )
  )

  ;; =======================================================================
  ;; 7. 무슨 일이 있어도 그룹화는 무조건 실행시켜 고아 객체 발생 차단
  ;; =======================================================================
  (tsp-group-last-entities last-ent boundary-ent)

  (if boundary-ent
    (vl-catch-all-apply
      '(lambda ( / v-bnd ms dict sort-ents arr)
         (setq v-bnd (vlax-ename->vla-object boundary-ent))
         (setq ms (vla-ObjectIdToObject (vla-get-Document v-bnd) (vla-get-OwnerId v-bnd)))
         (setq dict (vla-GetExtensionDictionary ms))
         (setq sort-ents (vl-catch-all-apply 'vla-GetObject (list dict "ACAD_SORTENTS")))
         (if (vl-catch-all-error-p sort-ents)
           (setq sort-ents (vla-AddObject dict "ACAD_SORTENTS" "AcDbSortentsTable"))
         )
         (setq arr (vlax-make-safearray vlax-vbobject '(0 . 0)))
         (vlax-safearray-put-element arr 0 v-bnd)
         (vla-MoveToTop sort-ents arr)
       )
    )
  )

  (vl-catch-all-apply 'vla-regen (list (vla-get-activedocument (vlax-get-acad-object)) 1))

  ;; 8. 내부에서 충돌(Crash)이 발생했다면, 커맨드창에 붉은 글로 에러 내용 보고
  (if (vl-catch-all-error-p draw-result)
    (princ (strcat "\n[에러 알림] 버팀보 작도 중 내부 충돌이 발생했습니다. (객체 추적은 정상 보존됨)\n상세 원인: " (vl-catch-all-error-message draw-result)))
    (princ "\n평면도 생성 및 갱신 완료.")
  )
)

;;;;==========================================================================
;;;; defun tsp-ceil :  숫자 올림 보조 함수
;;;;==========================================================================
(defun tsp-ceil (val)
  (if (= (type val) 'INT)
    val
    (if (= val (float (fix val)))
      (fix val)
      (1+ (fix val))
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-create-cip-guideline : 외측 방향을 고려한 D/2 오프셋 보조선 생성
;;;;==========================================================================
(defun tsp-create-cip-guideline (boundary-ent dia boundary-orient is-closed / vobj d start-pt p2 seg-angle out-norm target-pt off-obj off-ent closest-pt)
  (setq vobj (vlax-ename->vla-object boundary-ent))
  ;; dia가 문자열인지 숫자인지 판별하여 안전하게 처리 (에러 방지)
  (setq d (/ (if (= (type dia) 'STR) (atof dia) dia) 2.0))
  ;; H-Pile 로직에서 추출한 완벽한 외측 벡터 계산 (안팎 판별 오류 방지)
  (setq start-pt (vlax-curve-getStartPoint vobj))
  (setq p2 (vlax-curve-getPointAtParam vobj 1.0))
  (setq seg-angle (angle start-pt p2))
  (setq out-norm (+ seg-angle (* boundary-orient (/ pi 2.0))))
  ;; H-Pile과 완벽히 동일한 방식: 바깥쪽(흙 방향)을 향하도록 마이너스(-d) 적용
  (setq target-pt (polar start-pt out-norm (- d)))
  ;; 양수 거리로 오프셋 후, 수학적으로 계산된 target-pt와 일치하는지 교차 검증
  (setq off-obj (vl-catch-all-apply 'vla-offset (list vobj d)))
  (if (not (vl-catch-all-error-p off-obj))
    (progn
      (setq off-ent (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value off-obj) 0)))
      ;; 오프셋선 방향이 강제 반전되었을 때 시작점(off-start)이 달라지는 현상을 막기 위해,
      ;; target-pt와 가장 가까운 실제 폴리선 상의 점(closest-pt)을 도출하여 검증합니다.
      (setq closest-pt (vlax-curve-getClosestPointTo (vlax-ename->vla-object off-ent) target-pt))
      (if (> (distance (list (car target-pt) (cadr target-pt)) (list (car closest-pt) (cadr closest-pt))) 0.1)
        (progn
          (entdel off-ent)
          (setq off-obj (vl-catch-all-apply 'vla-offset (list vobj (- d))))
          (if (not (vl-catch-all-error-p off-obj))
            (setq off-ent (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value off-obj) 0)))
            (setq off-ent nil)
          )
        )
      )
      off-ent
    )
    nil
  )
)

;;;;==========================================================================
;;;; defun tsp-calc-cip-layout : 경계선(곡선)을 따라 C.I.P 중심점 배열 및 간격 계산
;;;;==========================================================================
(defun tsp-calc-cip-layout (curve-ent is-closed dia mode overlap / total-length target-pitch count actual-pitch i dist check-dist pt ang deriv centers next-dist)
  (setq total-length (vlax-curve-getdistatparam curve-ent (vlax-curve-getendparam curve-ent)))
  (setq target-pitch (if (= mode "0") dia (- dia overlap))) ; 0: 맞닿음(Tangent), 1: 겹침(Secant)

  ;; 폐합 여부에 따라 실제 배치 간격(actual-pitch)을 균등 분배하여 재계산
  (if is-closed
    (progn
      ;; 총 길이를 타겟 간격으로 나누고 반올림(0.5 더한 후 fix)하여 정확한 배치 개수 도출
      (setq count (fix (+ (/ total-length target-pitch) 0.5)))
      ;; 자투리 길이를 분산시킨 완벽한 간격 재계산
      (setq actual-pitch (/ total-length count))
    )
    (progn
      ;; 열린 경계선은 기존의 타겟 간격을 그대로 사용
      (setq actual-pitch target-pitch)
    )
  )

  (setq centers '())

  (if is-closed
    (progn
      ;; 닫힌 경계 (Closed boundary)
      (setq dist 0.0)
      (setq i 0)
      ;; 무한 루프나 겹침 초과를 방지하기 위해 계산된 count까지만 배열하도록 조건(< i count) 추가
      (while (and dist (< dist total-length) (< i count))
        (setq pt (vlax-curve-getpointatdist curve-ent dist))
        ;; 꼭짓점 접선각 오류 방지를 위한 미세 보정 (eps = 1e-6)
        (setq check-dist (+ dist 1e-6))
        (if (> check-dist total-length)
          (setq check-dist (- dist 1e-6))
        )
        (setq deriv (vlax-curve-getfirstderiv curve-ent (vlax-curve-getparamatdist curve-ent check-dist)))
        (setq ang (angle '(0 0 0) deriv))

        ;; [Phase 1] cons 최적화
        (setq centers (cons (list i dist pt ang) centers))

        ;; 다음 C.I.P 중심점 찾기 (균등 분배된 'actual-pitch' 기준 현의 길이 탐색)
        (setq next-dist (tsp-find-next-dist-by-chord curve-ent dist actual-pitch total-length))
        (setq dist next-dist)
        (setq i (1+ i))
      )
      (setq count i)
    )
    (progn
      ;; 열린 경계 (Open boundary)
      (setq dist (/ dia 2.0)) ; 양끝 D/2 보정 적용
      (setq i 0)
      (while (and dist (<= dist (- total-length (/ dia 2.0))))
        (setq pt (vlax-curve-getpointatdist curve-ent dist))
        ;; 꼭짓점 접선각 오류 방지를 위한 미세 보정 (eps = 1e-6)
        (setq check-dist (+ dist 1e-6))
        (if (> check-dist total-length)
          (setq check-dist (- dist 1e-6))
        )
        (setq deriv (vlax-curve-getfirstderiv curve-ent (vlax-curve-getparamatdist curve-ent check-dist)))
        (setq ang (angle '(0 0 0) deriv))

        ;; [Phase 1] cons 최적화
        (setq centers (cons (list i dist pt ang) centers))

        ;; 다음 C.I.P 중심점 찾기 (열린 구간은 타겟 간격과 동일한 'actual-pitch' 적용)
        (setq next-dist (tsp-find-next-dist-by-chord curve-ent dist actual-pitch total-length))
        (setq dist next-dist)
        (setq i (1+ i))
      )
      (setq count i)
    )
  )

  ;; [Phase 1] 뒤집기 완료
  (setq centers (reverse centers))

  (list
    (cons 'MODE (if is-closed "CLOSED" "OPEN"))
    (cons 'COUNT count)
    (cons 'ACTUAL-PITCH actual-pitch)
    (cons 'ITEMS centers)
  )
)

;;;;==========================================================================
;;;; defun tsp-draw-cip-elements : C.I.P 배열 요소를 작도하고, 브래킷을 생성하는 함수
;;;;==========================================================================
(defun tsp-draw-cip-elements (layout dia interval-idx hpile-idx boundary-orient / interval items hpile-spec-name hpile-vals h b tw tf hpile-block idx dist pt ang outward-normal hpile-rotation insert-pt half-h-offset in-norm p-start-center p-end-center p1 p2 p3 p4 br-layer br-color br-pl hatch-obj mspace doc sa err-eval old-hpannotative old-meas err-hatch)

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

  (setq interval (cond ((= interval-idx "1") 2) ((= interval-idx "2") 3) (t 1)))
  (setq items (cdr (assoc 'ITEMS layout)))

  (setq hpile-spec-name (nth (atoi hpile-idx) *tsp-std-wall-list*))
  (setq hpile-vals (parse-h-spec hpile-spec-name))
  (setq h (nth 0 hpile-vals) b (nth 1 hpile-vals) tw (nth 2 hpile-vals) tf (nth 3 hpile-vals))

  (setq hpile-block (create-hpile-section-block h b tw tf))

  (create-layer-if-not-exists "_CIP" "2")
  (setq br-layer "_브래킷(bracket)")
  (setq br-color 8)
  (create-layer-if-not-exists br-layer (itoa br-color))

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (if (= (getvar "CVPORT") 1)
    (setq mspace (vla-get-paperspace doc))
    (setq mspace (vla-get-modelspace doc))
  )

  (foreach item items
    (setq idx (nth 0 item) dist (nth 1 item) pt (nth 2 item) ang (nth 3 item))

    (entmake (list '(0 . "CIRCLE") (cons 8 "_CIP") (list 10 (car pt) (cadr pt) 0.0) (cons 40 (/ (if (= (type dia) 'STR) (atof dia) dia) 2.0))))

    (if (= (rem idx interval) 0)
      (progn
        (setq outward-normal (+ ang (* boundary-orient (/ pi 2.0))))
        (setq hpile-rotation (+ outward-normal (/ pi 2.0)))
        (setq half-h-offset (/ (if (= (type h) 'STR) (atof h) h) 2.0))
        (setq insert-pt (polar pt outward-normal half-h-offset))
        (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (list 10 (car insert-pt) (cadr insert-pt) 0.0) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation)))

        (setq err-eval (vl-catch-all-apply '> (list dia h)))

        (if (not (vl-catch-all-error-p err-eval))
          (if err-eval
            (progn
              ;; 띠장 방향 계산
              (setq in-norm outward-normal)
              (setq p-start-center (polar pt in-norm (/ h 2.0)))
              (setq p-end-center (polar pt in-norm (/ dia 2.0)))
              (setq p1 (polar p-start-center (+ in-norm (/ pi 2.0)) (/ b 2.0)))
              (setq p2 (polar p-end-center (+ in-norm (/ pi 2.0)) (/ b 2.0)))
              (setq p3 (polar p-end-center (- in-norm (/ pi 2.0)) (/ b 2.0)))
              (setq p4 (polar p-start-center (- in-norm (/ pi 2.0)) (/ b 2.0)))

              (setq br-pl (draw-bracket-poly (list p1 p2 p3 p4) br-layer br-color))
              (if br-pl
                (progn
                  (setq old-hpannotative (getvar "HPANNOTATIVE"))
                  (if old-hpannotative (setvar "HPANNOTATIVE" 0))

                  (setq err-hatch (vl-catch-all-apply 'vla-addhatch (list mspace 0 "ANSI31" :vlax-true)))
                  (if (not (vl-catch-all-error-p err-hatch))
                    (progn
                      (setq hatch-obj err-hatch)
                      (vl-catch-all-apply 'vla-put-patternscale (list hatch-obj (if (= (getvar "MEASUREMENT") 0) 400.0 20.0)))
                      (vl-catch-all-apply 'vla-put-layer (list hatch-obj br-layer))
                      (vl-catch-all-apply 'vla-put-color (list hatch-obj br-color))
                      (setq sa (vlax-make-safearray 9 (cons 0 0)))
                      (vlax-safearray-fill sa (list (vlax-ename->vla-object br-pl)))
                      (vl-catch-all-apply 'vla-appendouterloop (list hatch-obj sa))
                      (vl-catch-all-apply 'vla-evaluate (list hatch-obj))
                    )
                  )

                  (if old-hpannotative (setvar "HPANNOTATIVE" old-hpannotative))
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-acos : 아크코사인 함수 (수학 연산 지원)
;;;;==========================================================================
(defun tsp-acos (x)
  (cond
    ((>= x 1.0) 0.0)
    ((<= x -1.0) pi)
    (t (atan (sqrt (- 1.0 (* x x))) x))
  )
)

;;;;==========================================================================
;;;; defun tsp-normalize-angle : 각도를 0 ~ 2pi 사이로 정규화
;;;;==========================================================================
(defun tsp-normalize-angle (a)
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  a
)
