;;; ==========================================================================
;;; 파일명: TSP_5R_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part R)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-manual-delete-post, tsp-reset-post-manual]
;;; 포함 함수: tsp-manual-delete-post, tsp-reset-post-manual
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-manual-delete-post : 다중 영역/드래그 선택 및 연속 삭제
;;;;==========================================================================
(defun tsp-manual-delete-post (active-seg-id target-category / ss i ent ent-data blk-name pt-orig ins-pt p1 p2 ss-dup j dup-ent dup-data del-count loop valid-triangles ss-struts e-data xdata x-vals t-p1 t-p2 t-p3 is-inside tri skipped-count check-id e-xdata is-correct-cat post-tag)
  (princ (strcat "\n[안내] Seg-" (itoa active-seg-id) " 수동 삭제 작업을 시작합니다."))
  (setq loop T)

  (setq valid-triangles '())
  (setq ss-struts (ssget "X" '((0 . "LWPOLYLINE") (8 . "_사보강재(Corner_Strut),_버팀보(Strut)"))))
  (if ss-struts
    (progn
      (setq j 0)
      (while (< j (sslength ss-struts))
        (setq e-data (entget (ssname ss-struts j) '("TSP_STRUT_SPEC")))
        (setq cur-layer (cdr (assoc 8 e-data)))
        (setq xdata (assoc -3 e-data))
        (if xdata
          (progn
            (setq x-vals (cdr (car (cdr xdata))))
            (if (>= (length x-vals) 8)
              (progn
                (setq check-id (cdr (nth 0 x-vals)))
                (if (= check-id active-seg-id)
                  (progn
                    ;; 카테고리에 맞는 영역(레이어)만 스캔하도록 필터링
                    (if (and (= target-category "BRACE") (= cur-layer "_사보강재(Corner_Strut)"))
                      (progn
                        (setq t-p1 (cdr (nth 5 x-vals)))
                        (setq t-p2 (cdr (nth 6 x-vals)))
                        (setq t-p3 (cdr (nth 7 x-vals)))
                        (setq valid-triangles (append valid-triangles (list (list "TRI" t-p1 t-p2 t-p3))))
                      )
                    )
                    (if (and (= target-category "STRUT") (= cur-layer "_버팀보(Strut)"))
                      (progn
                        (setq s-p1 (cdr (nth 2 x-vals)))
                        (setq s-p2 (cdr (nth 3 x-vals)))
                        (setq s-width (cdr (nth 1 x-vals)))
                        (setq valid-triangles (append valid-triangles (list (list "RECT" s-p1 s-p2 s-width))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq j (1+ j))
      )
    )
  )

  (while loop
    (princ (strcat "\n[수동 삭제] Seg-" (itoa active-seg-id) " 구역 내의 객체를 선택하세요 (다중 선택/드래그 가능, 종료: Enter/ESC): "))
    (setq ss (vl-catch-all-apply 'ssget (list '((0 . "INSERT") (8 . "_중간말뚝(Post pile)")))))

    (cond
      ((vl-catch-all-error-p ss)
       (setq loop nil)
       (princ "\n[취소] 사용자가 ESC를 눌러 삭제 작업을 중단했습니다.")
      )
      ((not ss)
       (setq loop nil)
      )
      ((= (type ss) 'PICKSET)
       (setq i 0 del-count 0 skipped-count 0)
       (while (< i (sslength ss))
         (setq ent (ssname ss i))
         (if (entget ent)
           (progn
             (setq ent-data (entget ent))
             (setq blk-name (cdr (assoc 2 ent-data)))

             (if (wcmatch (strcase blk-name) "_POST_PILE*")
               (progn
                 (setq pt-orig (cdr (assoc 10 ent-data)))
                 (setq ins-pt (list (car pt-orig) (cadr pt-orig) 0.0))

                 ;; 선택된 객체가 타겟 카테고리의 XDATA를 가지고 있는지 우선 검사
                 (setq is-inside nil)
                 (setq e-xdata (assoc -3 (entget ent '("TSP_LISP_POST"))))
                 (setq is-correct-cat T)
                 (if e-xdata
                   (progn
                     (setq post-tag (cdr (nth 0 (cdr (car (cdr e-xdata))))))
                     (if (and post-tag (wcmatch post-tag "MANUAL_POST_*"))
                       (if (/= post-tag (strcat "MANUAL_POST_" target-category))
                         (setq is-correct-cat nil)
                       )
                     )
                   )
                 )

                 ;; XDATA가 일치할 때만 공간 스캔 수행
                 (if is-correct-cat
                   (foreach tri valid-triangles
                     (if (= (car tri) "TRI")
                       (if (tsp-is-point-in-triangle ins-pt (nth 1 tri) (nth 2 tri) (nth 3 tri)) (setq is-inside T))
                       (progn
                         (setq s-p1 (nth 1 tri) s-p2 (nth 2 tri) s-width (nth 3 tri))
                         (setq s-ang (angle s-p1 s-p2))
                         (setq d-proj (tsp-project-dist s-p1 s-ang ins-pt))
                         (setq d-perp (abs (- (* (- (car ins-pt) (car s-p1)) (sin s-ang)) (* (- (cadr ins-pt) (cadr s-p1)) (cos s-ang)))))
                         (if (and (>= d-proj -10.0) (<= d-proj (+ (distance s-p1 s-p2) 10.0)) (<= d-perp (+ s-width 100.0)))
                           (setq is-inside T)
                         )
                       )
                     )
                   )
                 )

                 (if is-inside
                   (progn
                     (if *tsp-use-temp-posts*
                       (setq *tsp-temp-deleted-posts* (append *tsp-temp-deleted-posts* (list ins-pt)))
                       (progn
                         (setq *tsp-deleted-post-points* (vlax-ldata-get "TSP_DICT" "DELETED_POSTS"))
                         (if (not *tsp-deleted-post-points*) (setq *tsp-deleted-post-points* '()))
                         (setq *tsp-deleted-post-points* (append *tsp-deleted-post-points* (list ins-pt)))
                         (vlax-ldata-put "TSP_DICT" "DELETED_POSTS" *tsp-deleted-post-points*)
                       )
                     )

                     (setq p1 (list (- (car ins-pt) 5.0) (- (cadr ins-pt) 5.0)))
                     (setq p2 (list (+ (car ins-pt) 5.0) (+ (cadr ins-pt) 5.0)))
                     (setq ss-dup (ssget "C" p1 p2 '((0 . "INSERT"))))
                     (if ss-dup
                       (progn
                         (setq j 0)
                         (while (< j (sslength ss-dup))
                           (setq dup-ent (ssname ss-dup j))
                           (if (entget dup-ent)
                             (progn
                               (setq dup-data (entget dup-ent))
                               (if (and (= (strcase (cdr (assoc 8 dup-data))) (strcase "_중간말뚝(Post pile)"))
                                        (wcmatch (strcase (cdr (assoc 2 dup-data))) "_POST_PILE*"))
                                 (entdel dup-ent)
                               )
                             )
                           )
                           (setq j (1+ j))
                         )
                       )
                     )

                     (if (entget ent) (entdel ent))
                     (setq del-count (1+ del-count))
                   )
                   (setq skipped-count (1+ skipped-count))
                 )
               )
             )
           )
         )
         (setq i (1+ i))
       )

       (setq *tsp-data-dirty* T)
       (command "_.REGEN")

       (if (> del-count 0) (princ (strcat "\n[완료] " (itoa del-count) "개의 중간말뚝이 삭제되었습니다.")))
       (if (> skipped-count 0) (princ (strcat "\n[오류 차단] 해당 구역 외부의 객체 " (itoa skipped-count) "개는 보호되어 삭제되지 않았습니다.")))
      )
    )
  )
  (princ "\n[종료] 삭제 완료.")
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-reset-post-manual : 중간말뚝 수동 삭제 및 추가 기록 모두 초기화 및 도면 객체 청소
;;;;==========================================================================
(defun tsp-reset-post-manual ( / ss i )
  ;; 1. 도면에 영구적으로 박제된(Bake) 중간말뚝 블록 객체를 모두 찾아 물리적 삭제
  (setq ss (ssget "X" '((0 . "INSERT") (8 . "_중간말뚝(Post pile)"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (entdel (ssname ss i))
        (setq i (1+ i))
      )
    )
  )

  ;; 2. 메모리 및 LDATA 기록 완전 삭제
  (if *tsp-use-temp-posts*
    (progn
      (setq *tsp-temp-added-posts* '())
      (setq *tsp-temp-deleted-posts* '())
    )
    (progn
      ;; 빈 리스트를 덮어쓰지 않고 LDATA 딕셔너리에서 항목 자체를 완전히 삭제
      (vlax-ldata-delete "TSP_DICT" "ADDED_POSTS")
      (vlax-ldata-delete "TSP_DICT" "DELETED_POSTS")
    )
  )
  (setq *tsp-data-dirty* T)

  ;; 3. 화면 갱신 (기존 말뚝이 모두 지워진 상태에서 자동 배치 설정된 말뚝들만 새로 깔끔하게 그려짐)
  (if (and (boundp '*tsp-boundary-ent*) *tsp-boundary-ent* (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*)
    (progn
      (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
      (princ "\n중간말뚝 추가/삭제 기록 초기화 및 객체 청소 완료")
    )
    (princ "\n 중간말뚝 추가/삭제 기록 초기화 및 객체 청소 완료")
  )
  (princ)
)
