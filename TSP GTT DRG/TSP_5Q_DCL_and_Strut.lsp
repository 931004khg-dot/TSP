;;; ==========================================================================
;;; 파일명: TSP_5Q_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part Q)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-manual-add-post, tsp-sanitize-support-list]
;;; 포함 함수: tsp-manual-add-post, tsp-sanitize-support-list
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-manual-add-post : 중간말뚝 수동추가
;;;;==========================================================================
(defun tsp-manual-add-post (active-seg-id target-category / old-osmode pt p-center chk_p1 chk_p2 ss-cross p1_box p2_box ss i ent ent-data p1 p2 min-dist line-ang perp-dist best-p1 best-p2 stiff-dir sum-x sum-y count centroid vec_dir vec_c2p cross_z dot_z rot ins_pt post-block added-list session-ents last-act loop custom-h custom-b custom-tw custom-tf target-seg-id ss-all-struts valid-strut-found j e-data xdata x-vals t-p1 t-p2 t-p3 post-data-record check-id first-run pure-b cur-layer s-p1 s-p2 s-width s-ang d-proj d-perp y_scale brace-xd brace-ang-ok bx-vals pt-cnt tmp-pa tmp-pb tmp-ang ang-diff)
  (setq old-osmode (getvar "OSMODE"))
  (setvar "OSMODE" 32)

  (if active-seg-id
    (princ (strcat "\n[안내] Seg-" (itoa active-seg-id) " 수동 추가 작업을 시작합니다."))
    (princ "\n[안내] 중간말뚝 수동 추가 작업을 시작합니다. (원하는 버팀보 영역을 클릭하세요)")
  )
  (setq session-ents '() loop T first-run T)

  (while loop
    (initget "Undo")
    (setq pt (vl-catch-all-apply 'getpoint (list "\n[수동 추가] 중간말뚝을 생성할 교차점 클릭 [명령취소(U)] <종료: Enter/ESC>: ")))

    (cond
      ((vl-catch-all-error-p pt)
       (setq loop nil)
       (princ "\n[취소] 사용자가 ESC를 눌러 추가 작업을 중단했습니다.")
      )
      ((= pt "Undo")
       (setq first-run nil)
       (if session-ents
         (progn
           (setq last-act (car session-ents))
           (setq session-ents (cdr session-ents))
           (if (= (type last-act) 'ENAME) (if (entget last-act) (entdel last-act)))

           (if *tsp-use-temp-posts*
             (if *tsp-temp-added-posts*
               (setq *tsp-temp-added-posts* (reverse (cdr (reverse *tsp-temp-added-posts*))))
             )
             (progn
               (setq added-list (vlax-ldata-get "TSP_DICT" "ADDED_POSTS"))
               (if added-list
                 (progn
                   (setq added-list (reverse (cdr (reverse added-list))))
                   (vlax-ldata-put "TSP_DICT" "ADDED_POSTS" added-list)
                 )
               )
             )
           )
           (setq *tsp-data-dirty* T)
           (if (and (boundp '*tsp-boundary-ent*) *tsp-boundary-ent* (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*)
             (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
             (command "_.REGEN")
           )
           (princ "\n[실행] 직전에 추가한 중간말뚝을 취소(Undo)했습니다.")
         )
         (princ "\n[알림] 취소할 작업 내역이 없습니다.")
       )
      )
      ((not pt)
       (if first-run
         (setq first-run nil)
         (setq loop nil)
       )
      )
      ((listp pt)
       (setq first-run nil)
       (setq p-center (list (car pt) (cadr pt) 0.0))

       (setq custom-h 300.0 custom-b 300.0 custom-tw 10.0 custom-tf 15.0 target-seg-id nil)
       (setq ss-all-struts (ssget "X" '((0 . "LWPOLYLINE") (8 . "_사보강재(Corner_Strut),_버팀보(Strut)"))))
       (setq valid-strut-found nil)

       (if ss-all-struts
         (progn
           (setq j 0)
           (while (and (< j (sslength ss-all-struts)) (not valid-strut-found))
             (setq e-data (entget (ssname ss-all-struts j) '("TSP_STRUT_SPEC")))
             (setq cur-layer (cdr (assoc 8 e-data)))
             (setq xdata (assoc -3 e-data))
             (if xdata
               (progn
                 (setq x-vals (cdr (car (cdr xdata))))
                 (if (>= (length x-vals) 8)
                   (progn
                     (setq check-id (cdr (nth 0 x-vals)))
                     (if (or (null active-seg-id) (= check-id active-seg-id))
                       (progn
                         ;; 카테고리에 맞는 영역(레이어)만 스캔하도록 이중 필터링
                         (if (and (= target-category "BRACE") (= cur-layer "_사보강재(Corner_Strut)"))
                           (progn
                             (setq t-p1 (cdr (nth 5 x-vals)))
                             (setq t-p2 (cdr (nth 6 x-vals)))
                             (setq t-p3 (cdr (nth 7 x-vals)))
                             (if (tsp-is-point-in-triangle p-center t-p1 t-p2 t-p3)
                               (progn
                                 (setq valid-strut-found T target-seg-id check-id)
                                 (setq custom-h (cdr (nth 1 x-vals)) custom-b (cdr (nth 2 x-vals)) custom-tw (cdr (nth 3 x-vals)) custom-tf (cdr (nth 4 x-vals)))
                               )
                             )
                           )
                         )
                         (if (and (= target-category "STRUT") (= cur-layer "_버팀보(Strut)"))
                           (progn
                             (setq s-p1 (cdr (nth 2 x-vals)))
                             (setq s-p2 (cdr (nth 3 x-vals)))
                             (setq s-width (cdr (nth 1 x-vals)))
                             (setq s-ang (angle s-p1 s-p2))

                             (setq d-proj (tsp-project-dist s-p1 s-ang p-center))
                             (setq d-perp (abs (- (* (- (car p-center) (car s-p1)) (sin s-ang)) (* (- (cadr p-center) (cadr s-p1)) (cos s-ang)))))

                             (if (and (>= d-proj -10.0) (<= d-proj (+ (distance s-p1 s-p2) 10.0)) (<= d-perp (+ s-width 100.0)))
                               (progn
                                 (setq valid-strut-found T target-seg-id check-id)
                                 (setq pure-b (if (> (length x-vals) 9) (cdr (nth 9 x-vals)) (cdr (nth 4 x-vals))))
                                 (setq custom-h (cdr (nth 4 x-vals)) custom-b pure-b custom-tw (cdr (nth 5 x-vals)) custom-tf (cdr (nth 6 x-vals)))
                               )
                             )
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

       (if (not valid-strut-found)
         (princ (strcat "\n[오류] 현재 설정 중인 [Seg-" (if active-seg-id (itoa active-seg-id) "?") "]의 구역 내부가 아닙니다. 버팀보 또는 사보강재 영역 내부를 클릭해주세요."))
         (progn
           (setq chk_p1 (list (- (car p-center) 2.0) (- (cadr p-center) 2.0)))
           (setq chk_p2 (list (+ (car p-center) 2.0) (+ (cadr p-center) 2.0)))
           (setq ss-cross (ssget "C" chk_p1 chk_p2 '((0 . "*LINE"))))

           (if (and valid-strut-found s-ang s-p1 s-p2)
             ;; --- RECT(버팀보) 경로 ---
             (progn
               (if (and ss-cross (>= (sslength ss-cross) 2))
                 (progn
               (setq min-dist 1e99 best-p1 nil best-p2 nil sum-x 0.0 sum-y 0.0 count 0)
               (setq p1_box (list (- (car p-center) 500.0) (- (cadr p-center) 500.0)))
               (setq p2_box (list (+ (car p-center) 500.0) (+ (cadr p-center) 500.0)))
               (setq ss (ssget "C" p1_box p2_box '((0 . "LWPOLYLINE") (8 . "_버팀보(Strut)"))))
               (if ss
                 (progn
                   (setq i 0)
                   (while (< i (sslength ss))
                     (setq ent (ssname ss i))
                     (setq ent-data (entget ent))
                     (setq brace-xd (assoc -3 ent-data))
                     (if (not brace-xd)
                       (progn
                         (setq p1 (cdr (assoc 10 ent-data)))
                         (setq p2 nil)
                         (setq pt-cnt 0)
                         (foreach dxf ent-data
                           (if (= (car dxf) 10)
                             (progn
                               (if (= pt-cnt 0) (setq p1 (cdr dxf)))
                               (if (= pt-cnt 1) (setq p2 (cdr dxf)))
                               (setq pt-cnt (1+ pt-cnt))
                             )
                           )
                         )
                         (if (and p1 p2)
                           (progn
                             (setq tmp-ang (angle p1 p2))
                             (setq ang-diff (abs (- tmp-ang s-ang)))
                             (if (> ang-diff pi) (setq ang-diff (- (* 2.0 pi) ang-diff)))
                             (if (or (and (> ang-diff (- (/ pi 2.0) 0.35)) (< ang-diff (+ (/ pi 2.0) 0.35)))
                                     (and (> ang-diff (- (* 1.5 pi) 0.35)) (< ang-diff (+ (* 1.5 pi) 0.35))))
                               (progn
                                 (setq tmp-pa (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
                                 (setq perp-dist (distance p-center tmp-pa))
                                 (if (< perp-dist min-dist)
                                   (progn
                                     (setq min-dist perp-dist)
                                     (setq best-p1 p1 best-p2 p2)
                                   )
                                 )
                                 (setq sum-x (+ sum-x (car tmp-pa)) sum-y (+ sum-y (cadr tmp-pa)) count (1+ count))
                               )
                             )
                           )
                         )
                       )
                     )
                     (setq i (1+ i))
                   )
                 )
               )
               (if (and best-p1 best-p2 (> count 0))
                 (progn
                   (setq centroid (list (/ sum-x count) (/ sum-y count) 0.0))
                   (setq stiff-dir (angle best-p1 best-p2))
                 )
                 (progn
                   (setq centroid (list (/ (+ (car s-p1) (car s-p2)) 2.0)
                                        (/ (+ (cadr s-p1) (cadr s-p2)) 2.0) 0.0))
                   (setq stiff-dir (+ s-ang (/ pi 2.0)))
                 )
               )
               (setq vec_dir (list (cos stiff-dir) (sin stiff-dir)))
               (setq vec_c2p (list (- (car p-center) (car centroid))
                                    (- (cadr p-center) (cadr centroid))))
               (setq cross_z (- (* (car vec_dir) (cadr vec_c2p)) (* (cadr vec_dir) (car vec_c2p))))
               (setq dot_z   (+ (* (car vec_dir) (car vec_c2p)) (* (cadr vec_dir) (cadr vec_c2p))))
               (if (>= dot_z 0.0) (setq rot stiff-dir) (setq rot (+ stiff-dir pi)))
               (if (< (* cross_z dot_z) 0.0) (setq y_scale -1.0) (setq y_scale 1.0))
               (setq ins_pt (polar p-center rot (/ custom-b 2.0)))
               (setq post-block (create-post-pile-block custom-h custom-b custom-tw custom-tf))

               ;; 레코드 마지막에 target-category 저장
               (setq post-data-record (list ins_pt rot y_scale custom-h custom-b custom-tw custom-tf target-seg-id target-category))

               (if *tsp-use-temp-posts*
                 (if *tsp-temp-deleted-posts*
                   (setq *tsp-temp-deleted-posts*
                     (vl-remove-if
                       '(lambda (del-pt) (< (distance ins_pt del-pt) 10.0))
                       *tsp-temp-deleted-posts*
                     )
                   )
                 )
                 (progn
                   (setq *tsp-deleted-post-points* (vlax-ldata-get "TSP_DICT" "DELETED_POSTS"))
                   (if *tsp-deleted-post-points*
                     (progn
                       (setq *tsp-deleted-post-points*
                         (vl-remove-if
                           '(lambda (del-pt) (< (distance ins_pt del-pt) 10.0))
                           *tsp-deleted-post-points*
                         )
                       )
                       (vlax-ldata-put "TSP_DICT" "DELETED_POSTS" *tsp-deleted-post-points*)
                     )
                   )
                 )
               )

               (if *tsp-use-temp-posts*
                 (setq *tsp-temp-added-posts* (append *tsp-temp-added-posts* (list post-data-record)))
                 (progn
                   (setq added-list (vlax-ldata-get "TSP_DICT" "ADDED_POSTS"))
                   (if (not added-list) (setq added-list '()))
                   (setq added-list (append added-list (list post-data-record)))
                   (vlax-ldata-put "TSP_DICT" "ADDED_POSTS" added-list)
                 )
               )
               (setq *tsp-data-dirty* T)
               (if (and (boundp '*tsp-boundary-ent*) *tsp-boundary-ent* (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*)
                 (progn
                   (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
                   (setq session-ents (cons 'REDRAW session-ents))
                 )
                 (progn
                   (if (not (tblsearch "APPID" "TSP_LISP_POST")) (regapp "TSP_LISP_POST"))
                   ;; MANUAL_POST 뒤에 카테고리 명찰 부착
                   (entmake (list '(0 . "INSERT") (cons 2 post-block) (cons 8 "_중간말뚝(Post pile)") (cons 62 256) (cons 10 ins_pt) '(41 . 1.0) (cons 42 y_scale) '(43 . 1.0) (cons 50 rot)
                                  (list -3 (list "TSP_LISP_POST" (cons 1000 (strcat "MANUAL_POST_" target-category))))
                             ))
                   (setq session-ents (cons (entlast) session-ents))
                 )
               )
               (princ "\n[완료] 중간말뚝 추가 완료.")
                 )
                 (princ "\n[오류] 두 선이 만나는 정확한 '교차점'이 아닙니다. 스냅을 이용해 다시 클릭하세요.")
               )
             )
             ;; --- TRI(사보강재) 경로: 기존 brace 검색 로직 ---
             (progn
           (if (and ss-cross (>= (sslength ss-cross) 2))
             (progn
               (setq min-dist 1e99 best-p1 nil best-p2 nil sum-x 0.0 sum-y 0.0 count 0)
               (setq p1_box (list (- (car p-center) 500.0) (- (cadr p-center) 500.0)))
               (setq p2_box (list (+ (car p-center) 500.0) (+ (cadr p-center) 500.0)))
               (setq ss (ssget "C" p1_box p2_box '((0 . "LWPOLYLINE") (8 . "_보강재(Brace)"))))

               (if ss
                 (progn
                   (setq i 0)
                   (while (< i (sslength ss))
                     (setq ent (ssname ss i))
                     (setq ent-data (entget ent '("TSP_LISP_BRACE")))
                     (setq p1 nil p2 nil)
                     (setq brace-xd (assoc -3 ent-data))
                     (setq brace-ang-ok T)
                     (if brace-xd
                       (progn
                         (setq bx-vals (cdr (car (cdr brace-xd))))
                         (if (and (>= (length bx-vals) 8)
                                  (= (cdr (nth 7 bx-vals)) "MID_GAP"))
                           (setq brace-ang-ok nil)
                           (progn
                             (setq pt-cnt 0)
                             (foreach item ent-data
                               (if (and (= (car item) 10) (< pt-cnt 2))
                                 (progn
                                   (if (= pt-cnt 0) (setq tmp-pa (cdr item)) (setq tmp-pb (cdr item)))
                                   (setq pt-cnt (1+ pt-cnt))
                                 )
                               )
                             )
                             (if (and tmp-pa tmp-pb)
                               (progn
                                 (setq tmp-ang (angle (list (car tmp-pa) (cadr tmp-pa) 0.0)
                                                      (list (car tmp-pb) (cadr tmp-pb) 0.0)))
                                 (if s-ang
                                   (progn
                                     (setq ang-diff (abs (- (rem (abs tmp-ang) pi) (rem (abs s-ang) pi))))
                                     (if (> ang-diff (/ pi 2.0)) (setq ang-diff (- pi ang-diff)))
                                     (if (> ang-diff (/ pi 12.0)) (setq brace-ang-ok nil))
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                     (if brace-ang-ok
                       (progn
                         (foreach item ent-data
                           (if (= (car item) 10)
                             (progn
                               (if (null p1) (setq p1 (cdr item)) (if (null p2) (setq p2 (cdr item))))
                               (setq sum-x (+ sum-x (car (cdr item))))
                               (setq sum-y (+ sum-y (cadr (cdr item))))
                               (setq count (1+ count))
                             )
                           )
                         )
                         (if (and p1 p2)
                           (progn
                             (setq p1 (list (car p1) (cadr p1) 0.0))
                             (setq p2 (list (car p2) (cadr p2) 0.0))
                             (setq line-ang (angle p1 p2))
                             (setq perp-dist (abs (- (* (- (car p-center) (car p1)) (sin line-ang)) (* (- (cadr p-center) (cadr p1)) (cos line-ang)))))
                             (if (< perp-dist min-dist)
                               (progn
                                 (setq min-dist perp-dist)
                                 (setq best-p1 p1 best-p2 p2)
                               )
                             )
                           )
                         )
                       )
                     )
                     (setq i (1+ i))
                   )

                   (if (and best-p1 best-p2 (> count 0) (< min-dist 10.0))
                     (progn
                       (setq centroid (list (/ sum-x count) (/ sum-y count) 0.0))
                       (setq stiff-dir (angle best-p1 best-p2))
                       (setq vec_dir (list (cos stiff-dir) (sin stiff-dir)))
                       (setq vec_c2p (list (- (car p-center) (car centroid)) (- (cadr p-center) (cadr centroid))))

                       (setq cross_z (- (* (car vec_dir) (cadr vec_c2p)) (* (cadr vec_dir) (car vec_c2p))))
                       (setq dot_z (+ (* (car vec_dir) (car vec_c2p)) (* (cadr vec_dir) (cadr vec_c2p))))

                       (if (>= dot_z 0.0) (setq rot stiff-dir) (setq rot (+ stiff-dir pi)))
                       (if (< (* cross_z dot_z) 0.0) (setq y_scale -1.0) (setq y_scale 1.0))

                       (setq ins_pt (polar p-center rot (/ custom-b 2.0)))
                       (setq post-block (create-post-pile-block custom-h custom-b custom-tw custom-tf))

                       ;; 레코드 마지막에 target-category 저장
                       (setq post-data-record (list ins_pt rot y_scale custom-h custom-b custom-tw custom-tf target-seg-id target-category))

                       (if *tsp-use-temp-posts*
                         (if *tsp-temp-deleted-posts*
                           (setq *tsp-temp-deleted-posts*
                             (vl-remove-if
                               '(lambda (del-pt) (< (distance ins_pt del-pt) 10.0))
                               *tsp-temp-deleted-posts*
                             )
                           )
                         )
                         (progn
                           (setq *tsp-deleted-post-points* (vlax-ldata-get "TSP_DICT" "DELETED_POSTS"))
                           (if *tsp-deleted-post-points*
                             (progn
                               (setq *tsp-deleted-post-points*
                                 (vl-remove-if
                                   '(lambda (del-pt) (< (distance ins_pt del-pt) 10.0))
                                   *tsp-deleted-post-points*
                                 )
                               )
                               (vlax-ldata-put "TSP_DICT" "DELETED_POSTS" *tsp-deleted-post-points*)
                             )
                           )
                         )
                       )

                       (if *tsp-use-temp-posts*
                         (setq *tsp-temp-added-posts* (append *tsp-temp-added-posts* (list post-data-record)))
                         (progn
                           (setq added-list (vlax-ldata-get "TSP_DICT" "ADDED_POSTS"))
                           (if (not added-list) (setq added-list '()))
                           (setq added-list (append added-list (list post-data-record)))
                           (vlax-ldata-put "TSP_DICT" "ADDED_POSTS" added-list)
                         )
                       )

                       (setq *tsp-data-dirty* T)

                       (if (and (boundp '*tsp-boundary-ent*) *tsp-boundary-ent* (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*)
                         (progn
                           (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
                           (setq session-ents (cons 'REDRAW session-ents))
                         )
                         (progn
                           (if (not (tblsearch "APPID" "TSP_LISP_POST")) (regapp "TSP_LISP_POST"))
                           ;; MANUAL_POST 뒤에 카테고리 명찰 부착
                           (entmake (list '(0 . "INSERT") (cons 2 post-block) (cons 8 "_중간말뚝(Post pile)") (cons 62 256) (cons 10 ins_pt) '(41 . 1.0) (cons 42 y_scale) '(43 . 1.0) (cons 50 rot)
                                          (list -3 (list "TSP_LISP_POST" (cons 1000 (strcat "MANUAL_POST_" target-category))))
                                    ))
                           (setq session-ents (cons (entlast) session-ents))
                         )
                       )
                       (princ "\n[완료] 중간말뚝 추가 완료.")
                     )
                     (princ "\n[오류] 클릭한 교차점에 보강재(Brace)가 지나가지 않습니다.")
                   )
                 )
                 (princ "\n[오류] 반경 내에 보강재(Brace) 객체가 없습니다.")
               )
             )
             (princ "\n[오류] 두 선이 만나는 정확한 '교차점'이 아닙니다. 스냅을 이용해 다시 클릭하세요.")
           )
             )
           )
         )
       )
      )
    )
  )

  (setvar "OSMODE" old-osmode)
  (princ "\n[종료] 수동 추가 완료.")
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-sanitize-support-list : 지보재 리스트 내 오염 데이터 제거 및 ARX 크래시 방지용 강제 복원
;;;;==========================================================================
(defun tsp-sanitize-support-list (sup-list / clean-list item-type new-item opt)
  (setq clean-list '())
  (if sup-list
    (foreach item sup-list
      (if (and (listp item) (> (length item) 0))
        (progn
          (setq item-type (car item))
          ;; ADD_POST 등 제어 문자열이 포함된 패킷은 제외하고 정상 데이터만 필터링
          (if (and item-type (= (type item-type) 'STR) (not (wcmatch item-type "*_POST")))
            (progn
              (setq new-item item)
              ;; [ARX 크래시 핵심 방어] 버팀보 데이터 중 보강재 옵션(11번째 인덱스)이 과거 버그로 인해 문자열로 오염된 경우 리스트로 강제 복원
              (if (wcmatch item-type "버팀보*")
                (if (> (length item) 11)
                  (progn
                    (setq opt (nth 11 item))
                    (if (= (type opt) 'STR)
                      (progn
                        ;; 문자열 상태의 옵션("(\"1\" \"0\"...)")을 진짜 리스트로 파싱
                        (setq opt (read opt))
                        ;; 안전한 리스트로 파싱된 데이터를 원래 위치에 끼워넣어 정상 레코드로 재조립 (12, 13번 인덱스 보존)
                        (setq new-item (update-list-item item 11 opt))
                      )
                    )
                  )
                )
              )
              (setq clean-list (append clean-list (list new-item)))
            )
          )
        )
      )
    )
  )
  clean-list
)
