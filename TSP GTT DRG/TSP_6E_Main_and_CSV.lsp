;;; ==========================================================================
;;; 파일명: TSP_6E_Main_and_CSV.lsp
;;; 구  분: 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part E)
;;; 역  할: C:TSP 메인 상태 머신, 실시간 평면 갱신(tsp-redraw-realtime-plan), 도면 N치 드래그 추출, CSV 내보내기/불러오기/매칭, 파이썬(GeoX) 다이렉트 연동, SUB-ZONES 전개 [tsp-extract-number-only, tsp-get-nvalue-from-drawing, tsp-open-manager-directly, tsp-direct-plan-from-python, tsp-direct-section-from-python 외 4개]
;;; 포함 함수: tsp-extract-number-only, tsp-get-nvalue-from-drawing, tsp-open-manager-directly, tsp-direct-plan-from-python, tsp-direct-section-from-python, tsp-get-zone-wall-stiffness, tsp-resolve-wall-spans, tsp-expand-segments-for-wall-plan, tsp-log
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-extract-number-only : 텍스트에서 숫자와 소수점('.')만 추출
;;;;==========================================================================
(defun tsp-extract-number-only (str / i char res)
  (setq res "")
  (setq i 1)
  (while (<= i (strlen str))
    (setq char (substr str i 1))
    (if (or (and (>= (ascii char) 48) (<= (ascii char) 57))
            (= (ascii char) 46))
      (setq res (strcat res char))
    )
    (setq i (1+ i))
  )
  (if (= res "") "0.0" res)
)

;;;;==========================================================================
;;;; defun tsp-get-nvalue-from-drawing : 도면에서 텍스트를 드래그하여 N치 추출
;;;;==========================================================================
(defun tsp-get-nvalue-from-drawing (/ ss i ent ent-data txt-str pt-x pt-y txt-h txt-list row-list current-row prev-y new-n-values depth-str depth-val n-text)
  (princ "\n[N치 자동입력] 깊이와 N치 텍스트들을 드래그하여 선택하세요: ")
  (setq ss (ssget '((0 . "TEXT"))))
  (if (not ss)
    (progn (princ "\n선택된 텍스트가 없습니다.") nil)
    (progn
      (if (= (rem (sslength ss) 2) 1)
        (progn
          (alert "선택된 텍스트의 총 개수가 홀수입니다.\n깊이와 N치가 짝수 개로 맞는지 확인하고 다시 선택해주세요.")
          nil
        )
        (progn
          (setq txt-list '() i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq ent-data (entget ent))
            (setq txt-str (cdr (assoc 1 ent-data)))
            (setq txt-h (cdr (assoc 40 ent-data)))
            (setq pt-x (car (cdr (assoc 10 ent-data))))
            (setq pt-y (cadr (cdr (assoc 10 ent-data))))
            (setq txt-list (append txt-list (list (list pt-y pt-x txt-str txt-h))))
            (setq i (1+ i))
          )
          (setq txt-list (vl-sort txt-list (function (lambda (a b) (> (car a) (car b))))))
          (setq row-list '() current-row '() prev-y (car (nth 0 txt-list)))
          (foreach item txt-list
            (setq pt-y (nth 0 item) txt-h (nth 3 item))
            (if (<= (abs (- prev-y pt-y)) (* txt-h 0.5))
              (setq current-row (append current-row (list item)))
              (progn
                (setq row-list (append row-list (list current-row)))
                (setq current-row (list item) prev-y pt-y)
              )
            )
          )
          (if current-row (setq row-list (append row-list (list current-row))))
          (setq new-n-values '())
          (foreach row row-list
            (if (= (length row) 2)
              (progn
                (setq row (vl-sort row (function (lambda (a b) (< (cadr a) (cadr b))))))
                (setq depth-str (tsp-extract-number-only (nth 2 (nth 0 row))))
                (setq depth-val (atof depth-str))
                (setq n-text (nth 2 (nth 1 row)))
                (setq new-n-values (append new-n-values (list (list depth-val n-text))))
              )
            )
          )
          (if (> (length new-n-values) 0)
            (progn
              (alert (strcat "총 " (itoa (length new-n-values)) "쌍의 N치 데이터가 성공적으로 추가되었습니다."))
              new-n-values
            )
            (progn
              (alert "유효한 N치 데이터를 찾을 수 없습니다.\n행의 짝(깊이, N치)이 맞게 선택되었는지 확인해주세요.")
              nil
            )
          )
        )
      )
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-open-manager-directly : 다이렉트 작도 완료 후 사용자가 원할 때 세그먼트 설정창(Manager)으로 즉시 진입
;;;;==========================================================================
(defun tsp-open-manager-directly (/ dcl-path loop-draw status save-chk proceed-draw perform-draw opt valid-sources idx s-data input-valid default-src src-num src-idx choice-res check-data b-data draw-seg draw-cor ans-cor limits brace-res new-brace abort-draw missing-brace-list i ans rem-count res ans2 midx mseg user-ans selected-idx)
  (setq dcl-path (create-tsp-dcl))
  (setq loop-draw T)
  (while loop-draw
    (setq status (manager-dialog-callback dcl-path))
    (cond
      ((= status 0)
       (setq loop-draw nil)
       (if *tsp-data-dirty*
         (progn
           (initget "Yes No")
           (setq save-chk (getkword (strcat "\n변경된 내용을 저장하시겠습니까? (프로젝트: " *tsp-current-project-name* ") [Yes(Y)/No(N)] <Y>: ")))
           (if (null save-chk) (setq save-chk "Yes"))
           (if (= save-chk "Yes")
             (progn
               (tsp-save-project-data *tsp-current-project-name*)
               (if *tsp-boundary-ent* (tsp-pm-tag-boundary *tsp-boundary-ent* *tsp-current-project-name*))
             )
             (princ "\n[알림] 저장하지 않고 종료합니다.")
           )
         )
       )
       (princ "\nTSP 설정창을 종료합니다.")
      )
      ((= status 5)
       (setq proceed-draw T perform-draw T)
       (if (draw-section-ground-line dcl-path selected-idx)
         (princ "\n단면도 작도 완료.")
         (progn (princ "\n단면도 작도 취소됨.") (setq proceed-draw nil))
       )
       (if proceed-draw
         (progn
           (initget "Yes No")
           (setq choice-res (vl-catch-all-apply 'getkword (list "\n설정창으로 돌아가시겠습니까? [Yes(Y)/No(N)] <Y>: ")))
           (if (vl-catch-all-error-p choice-res)
             (setq loop-draw nil)
             (if (= choice-res "No") (setq loop-draw nil))
           )
         )
       )
      )
      ((= status 6)
       (if (and *tsp-boundary-ent* *tsp-boundary-orient*)
         (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
       )
       (initget "Yes No")
       (setq user-ans (vl-catch-all-apply 'getkword (list "\n설정창으로 다시 돌아가시겠습니까? [Yes(Y)/No(N)] <Y>: ")))
       (if (vl-catch-all-error-p user-ans)
         (setq loop-draw nil)
         (if (= user-ans "No") (setq loop-draw nil))
       )
      )
      ((= status 16)
       (if (not (boundp 'drg-run-elevation-with-tsp))
         (if (findfile "DRG.LSP") (load (findfile "DRG.LSP")))
       )
       (if (boundp 'drg-run-elevation-with-tsp)
         (drg-run-elevation-with-tsp *segment-list* *tsp-boundary-ent* *tsp-boundary-orient*)
         (alert "DRG.LSP 파일을 찾을 수 없거나 로드되지 않았습니다.")
       )
       (initget "Yes No")
       (setq user-ans (vl-catch-all-apply 'getkword (list "\n설정창으로 다시 돌아가시겠습니까? [Yes(Y)/No(N)] <Y>: ")))
       (if (vl-catch-all-error-p user-ans)
         (setq loop-draw nil)
         (if (= user-ans "No") (setq loop-draw nil))
       )
      )
    )
  )
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-direct-plan-from-python : 파이썬 GUI에서 [평면도 즉시 작도] 클릭 시 실행
;;;;   - has-bnd=T : 파이썬에 이미 경계선 데이터(CAD 연동 Handle 또는 기존 CSV 좌표)가 있는 상태 -> 즉시 작도
;;;;   - has-bnd=nil : 가상 세그먼트 상태 -> CAD에서 경계선 선택 후 즉시 작도
;;;;   - 작도 완료 후 세그먼트 설정창 오픈 여부를 사용자에게 질문
;;;;==========================================================================
(defun tsp-direct-plan-from-python (csv-path has-bnd ent-handle start-pt orient / csv-segs bnd-ent bnd-orient vertices poly-orient start-idx dist-start dist-end curr-len csv-len i merged-list curr-seg csv-seg sel user-start-pt bnd-data open-mgr bnd-restore-verts ldata-orient seg-verts)
  (setvar "cmdecho" 0)
  (setq csv-segs (tsp-parse-csv-file-data csv-path))
  (if (null csv-segs)
    (alert "파이썬에서 전달된 단면 데이터를 읽을 수 없습니다.")
    (progn
      (setq *tsp-realtime-entities* nil)
      (setq *tsp-last-draw-state* nil)
      (setq bnd-ent nil)

      (if has-bnd
        ;; [분기 A] 파이썬에 경계선 데이터가 들어있는 상태
        (progn
          (if (and ent-handle (/= ent-handle "") (handent ent-handle) (entget (handent ent-handle)))
            (progn
              (setq bnd-ent (handent ent-handle))
              (tsp-fix-mirrored-polyline bnd-ent)
              (setq bnd-orient (if orient orient (determine-boundary-orientation bnd-ent)))
              (setq vertices (extract-vertices bnd-ent))
              (if (is-closed-polyline bnd-ent)
                (progn
                  (setq poly-orient (get-polygon-orientation vertices))
                  (if (= poly-orient 1) (setq vertices (reverse vertices)))
                  (if start-pt
                    (progn
                      (setq start-idx (tsp-get-closest-vertex-index vertices start-pt))
                      (setq vertices (tsp-shift-vertex-list vertices start-idx))
                      (vlax-ldata-put bnd-ent "TSP_START_IDX" start-idx)
                    )
                  )
                )
                (if start-pt
                  (if (< (distance (last vertices) start-pt) (distance (car vertices) start-pt))
                    (setq vertices (reverse vertices))
                  )
                )
              )
              (if start-pt (vlax-ldata-put bnd-ent "TSP_START_PT" start-pt))
              (vlax-ldata-put bnd-ent "TSP_ORIENT" bnd-orient)
              (setq *segment-list* nil)
              (init-segment-data bnd-ent bnd-orient vertices)

              (setq mapping (tsp-csv-mapping-dialog (strcat (getvar "TEMPPREFIX") "tsp.dcl") *segment-list* csv-segs))
              (if mapping
                (progn
                  (setq curr-len (length *segment-list*))
                  (setq merged-list '() i 0)
                  (while (< i curr-len)
                    (setq curr-seg (nth i *segment-list*))
                    (setq map-idx (nth i mapping))
                    (if map-idx
                      (progn
                        (setq csv-seg (nth map-idx csv-segs))
                        (setq merged-list (append merged-list (list (tsp-merge-csv-seg-into-curr curr-seg csv-seg))))
                      )
                      (setq merged-list (append merged-list (list curr-seg)))
                    )
                    (setq i (1+ i))
                  )
                  (setq *segment-list* merged-list)
                )
                (princ "\n[알림] 매칭 작업이 취소되어 단면 데이터가 갱신되지 않았습니다.")
              )
            )
            ;; 기존 CSV 로드 상태(Handle 없음) -> CSV 좌표로 경계선 자동 재구성
            (progn
              (setq *segment-list* csv-segs)
              ;; [패치] CSV에 #BOUNDARY(원본 경계선 정점)가 있으면 그 정점으로 폴리라인을
              ;; 재생성하여 자동삭제된 180도 근접 모서리(꺾임)를 완벽히 보존한다.
              (setq bnd-ent
                (if (and (boundp '*tsp-csv-boundary-data*) *tsp-csv-boundary-data*
                         (nth 1 *tsp-csv-boundary-data*) (> (length (nth 1 *tsp-csv-boundary-data*)) 1))
                  (progn
                    (setq bnd-restore-verts
                      (mapcar '(lambda (pt) (list (car pt) (cadr pt) 0.0)) (nth 1 *tsp-csv-boundary-data*)))
                    (entmake
                      (append
                        (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                              (cons 90 (length bnd-restore-verts))
                              (cons 70 (if (= (nth 0 *tsp-csv-boundary-data*) 1) 1 0)))
                        (mapcar '(lambda (pt) (list 10 (car pt) (cadr pt))) bnd-restore-verts))
                    )
                    (entlast)
                  )
                  (tsp-rebuild-boundary)
                )
              )
              (setq bnd-orient (if orient orient (determine-boundary-orientation bnd-ent)))
              (create-layer-if-not-exists "_경계선(Boundary)" "4")
              (setq bnd-data (entget bnd-ent))
              (setq bnd-data (subst (cons 8 "_경계선(Boundary)") (assoc 8 bnd-data) bnd-data))
              (if (assoc 62 bnd-data)
                (setq bnd-data (subst (cons 62 4) (assoc 62 bnd-data) bnd-data))
                (setq bnd-data (append bnd-data (list (cons 62 4))))
              )
              (entmod bnd-data)
              (setq user-start-pt (cdr (assoc 'V-START (nth 0 csv-segs))))
              (vlax-ldata-put bnd-ent "TSP_START_PT" user-start-pt)
              ;; 개방형 폴리라인의 원본 정점 순서가 1번 세그먼트 시작점과 역방향이면 TSP_ORIENT 부호 반전
              (setq vertices (extract-vertices bnd-ent))
              (setq ldata-orient
                (if (and (not (is-closed-polyline bnd-ent))
                         (> (length vertices) 1)
                         user-start-pt
                         (< (distance (last vertices) user-start-pt)
                            (distance (car vertices) user-start-pt)))
                  (- bnd-orient)
                  bnd-orient
                )
              )
              (vlax-ldata-put bnd-ent "TSP_ORIENT" ldata-orient)
              ;; 세그먼트 정점 구성(모서리 자동삭제/편집 결과)도 경계선에 함께 저장하여 정합성 유지
              (if csv-segs
                (progn
                  (setq seg-verts (mapcar '(lambda (s) (cdr (assoc 'V-START s))) csv-segs))
                  (if (not (is-closed-polyline bnd-ent))
                    (setq seg-verts (append seg-verts (list (cdr (assoc 'V-END (last csv-segs))))))
                  )
                  (tsp-save-custom-vertices-ldata bnd-ent seg-verts)
                )
              )
            )
          )
        )
        ;; [분기 B] 파이썬에 경계선 데이터가 없는 상태(가상 세그먼트) -> [기존경계선적용(B)/단면도전용(S)] 선택
        (progn
          (initget "Boundary Section")
          (setq py-opt (getkword "\n[GeoX -> AutoCAD] 경계선 데이터가 없습니다. 방식을 선택하세요 [기존경계선적용(B)/단면도전용(S)] <B>: "))
          (if (not py-opt) (setq py-opt "Boundary"))
          (if (= py-opt "Section")
            (progn
              (setq *segment-list* csv-segs)
              (setq *tsp-boundary-ent* nil)
              (setq *tsp-boundary-orient* nil)
              (tsp-save-project-data *tsp-current-project-name*)
              (setq bnd-ent "CANCEL")
              (princ "\n[단면도 전용 모드] 세그먼트 설정창으로 진입합니다.")
              (tsp-open-manager-directly)
            )
            (progn
              (princ "\n[GeoX -> AutoCAD 평면도 작도] 도면에서 단면을 적용할 경계선(Polyline)을 선택하세요...")
              (while (not bnd-ent)
                (initget "Exit")
                (setq sel (entsel "\n단면 데이터를 적용할 경계선(Polyline) 선택 [취소(Exit)]: "))
                (cond
                  ((= sel "Exit") (setq bnd-ent "CANCEL"))
                  ((/= sel nil) (setq bnd-ent (car sel)))
                  (t (princ "\n다시 선택해주세요."))
                )
              )
              (if (/= bnd-ent "CANCEL")
                (progn
                  (tsp-fix-mirrored-polyline bnd-ent)
                  (setq bnd-orient (determine-boundary-orientation bnd-ent))
                  (setq vertices (extract-vertices bnd-ent))
                  (setq user-start-pt (getpoint "\n세그먼트 시작점(No.1)이 될 위치를 클릭하세요: "))
                  (if user-start-pt
                    (progn
                      (setq user-start-pt (trans user-start-pt 1 0))
                      (vlax-ldata-put bnd-ent "TSP_START_PT" user-start-pt)
                    )
                  )
                  (if (is-closed-polyline bnd-ent)
                    (progn
                      (setq poly-orient (get-polygon-orientation vertices))
                      (if (= poly-orient 1)
                        (setq vertices (reverse vertices) bnd-orient (- bnd-orient))
                      )
                      (if user-start-pt
                        (progn
                          (setq start-idx (tsp-get-closest-vertex-index vertices user-start-pt))
                          (setq vertices (tsp-shift-vertex-list vertices start-idx))
                          (vlax-ldata-put bnd-ent "TSP_START_IDX" start-idx)
                        )
                      )
                    )
                    (if user-start-pt
                      (if (< (distance (last vertices) user-start-pt) (distance (car vertices) user-start-pt))
                        (setq vertices (reverse vertices) bnd-orient (- bnd-orient))
                      )
                    )
                  )
                  (setq *segment-list* nil)
                  (init-segment-data bnd-ent bnd-orient vertices)
                  (if (not (findfile (strcat (getvar "TEMPPREFIX") "tsp.dcl"))) (create-tsp-dcl))
                  (setq mapping (tsp-csv-mapping-dialog (strcat (getvar "TEMPPREFIX") "tsp.dcl") *segment-list* csv-segs))
                  (if mapping
                    (progn
                      (setq curr-len (length *segment-list*) merged-list '() i 0)
                      (while (< i curr-len)
                        (setq curr-seg (nth i *segment-list*))
                        (setq map-idx (nth i mapping))
                        (if map-idx
                          (setq merged-list (append merged-list (list (tsp-merge-csv-seg-into-curr curr-seg (nth map-idx csv-segs)))))
                          (setq merged-list (append merged-list (list curr-seg)))
                        )
                        (setq i (1+ i))
                      )
                      (setq *segment-list* merged-list)
                      (vlax-ldata-put bnd-ent "TSP_ORIENT" bnd-orient)
                    )
                    (setq bnd-ent "CANCEL")
                  )
                )
              )
            )
          )
        )
      )

      ;; 평면도 즉시 작도 및 설정창 진입 여부 확인
      (if (and bnd-ent (/= bnd-ent "CANCEL"))
        (progn
          (setq *tsp-boundary-ent* bnd-ent)
          (setq *tsp-boundary-orient* bnd-orient)
          (tsp-save-project-data *tsp-current-project-name*)
          (tsp-pm-tag-boundary bnd-ent *tsp-current-project-name*)
          (tsp-redraw-realtime-plan bnd-ent bnd-orient)
          (princ "\n[완료] GeoX 데이터가 반영된 가시설 평면도가 즉시 생성되었습니다.")
          (initget "Yes No")
          (setq open-mgr (vl-catch-all-apply 'getkword (list "\nTSP 세그먼트 설정창을 여시겠습니까? [Yes(Y)/No(N)] <Y>: ")))
          (if (not (vl-catch-all-error-p open-mgr))
            (if (or (null open-mgr) (= open-mgr "Yes"))
              (tsp-open-manager-directly)
            )
          )
        )
      )
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-direct-section-from-python : 파이썬 GUI에서 [단면도 즉시 작도] 클릭 시 실행
;;;;   - 지정된 세그먼트(및 세부 구간 subzone-id)의 단면도를 즉시 작도
;;;;==========================================================================
(defun tsp-direct-section-from-python (csv-path seg-idx subzone-id / csv-segs dcl-path open-mgr draw-ok)
  (setvar "cmdecho" 0)
  (setq csv-segs (tsp-parse-csv-file-data csv-path))
  (if (null csv-segs)
    (alert "파이썬에서 전달된 단면 데이터를 읽을 수 없습니다.")
    (progn
      (setq *segment-list* csv-segs)
      (if (or (null seg-idx) (< seg-idx 0) (>= seg-idx (length *segment-list*)))
        (setq seg-idx 0)
      )
      (load-segment-to-globals seg-idx)
      (setq *tsp-forced-subzone-id* (if (and subzone-id (> subzone-id 0)) subzone-id nil))
      (setq dcl-path (create-tsp-dcl))
      (princ (strcat "\n[GeoX -> AutoCAD 단면도 즉시 작도] Seg-" (itoa (1+ seg-idx))
                     (if *tsp-forced-subzone-id* (strcat " (제" (itoa *tsp-forced-subzone-id*) "구간)") "")
                     " 단면도를 작도합니다."))
      (setq draw-ok (draw-section-ground-line dcl-path seg-idx))
      (setq *tsp-forced-subzone-id* nil)
      (if draw-ok
        (progn
          (princ "\n[완료] 단면도 작도가 완료되었습니다.")
          (initget "Yes No")
          (setq open-mgr (vl-catch-all-apply 'getkword (list "\nTSP 세그먼트 설정창을 여시겠습니까? [Yes(Y)/No(N)] <Y>: ")))
          (if (not (vl-catch-all-error-p open-mgr))
            (if (or (null open-mgr) (= open-mgr "Yes"))
              (tsp-open-manager-directly)
            )
          )
        )
        (princ "\n[취소] 단면도 작도가 취소되었습니다.")
      )
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-direct-elevation-from-python : 파이썬 GUI에서 [전개도 즉시 작도] 클릭 시 실행
;;;;==========================================================================
(defun tsp-direct-elevation-from-python (csv-path has-bnd ent-handle start-pt orient / csv-segs bnd-ent)
  (setvar "cmdecho" 0)
  (setq csv-segs (tsp-parse-csv-file-data csv-path))
  (if (null csv-segs)
    (alert "파이썬에서 전달된 단면 데이터를 읽을 수 없습니다.")
    (progn
      (setq *segment-list* csv-segs)
      (setq bnd-ent nil)
      (if (and has-bnd ent-handle (/= ent-handle "") (handent ent-handle) (entget (handent ent-handle)))
        (progn
          (setq bnd-ent (handent ent-handle))
          (if start-pt (vlax-ldata-put bnd-ent "TSP_START_PT" start-pt))
          (if orient (vlax-ldata-put bnd-ent "TSP_ORIENT" orient))
          (setq *tsp-boundary-ent* bnd-ent)
          (setq *tsp-boundary-orient* orient)
        )
      )
      (if (not (boundp 'drg-run-elevation-with-tsp))
        (if (findfile "DRG.LSP") (load (findfile "DRG.LSP")))
      )
      (if (boundp 'drg-run-elevation-with-tsp)
        (drg-run-elevation-with-tsp *segment-list* bnd-ent orient)
        (alert "DRG.LSP 파일을 찾을 수 없거나 로드되지 않았습니다.")
      )
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-direct-elevation-from-python : 파이썬 GUI에서 [전개도 즉시 작도] 클릭 시 실행
;;;;==========================================================================
(defun tsp-direct-elevation-from-python (csv-path has-bnd ent-handle start-pt orient / csv-segs bnd-ent open-mgr)
  (setvar "cmdecho" 0)
  (setq csv-segs (tsp-parse-csv-file-data csv-path))
  (if (null csv-segs)
    (alert "파이썬에서 전달된 단면 데이터를 읽을 수 없습니다.")
    (progn
      (setq *segment-list* csv-segs)
      (setq bnd-ent nil)
      (if (and has-bnd ent-handle (/= ent-handle "") (handent ent-handle) (entget (handent ent-handle)))
        (progn
          (setq bnd-ent (handent ent-handle))
          (if start-pt (vlax-ldata-put bnd-ent "TSP_START_PT" start-pt))
          (if orient (vlax-ldata-put bnd-ent "TSP_ORIENT" orient))
          (setq *tsp-boundary-ent* bnd-ent)
          (setq *tsp-boundary-orient* orient)
        )
      )
      (if (not (boundp 'drg-run-elevation-with-tsp))
        (if (findfile "DRG.LSP") (load (findfile "DRG.LSP")))
      )
      (if (boundp 'drg-run-elevation-with-tsp)
        (drg-run-elevation-with-tsp *segment-list* bnd-ent orient)
        (alert "DRG.LSP 파일을 찾을 수 없거나 로드되지 않았습니다.")
      )
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-get-zone-wall-stiffness : 구간(또는 세그먼트) 벽체 강성 점수 산출
;;;;==========================================================================
(defun tsp-get-zone-wall-stiffness (z-data / w-type h-idx h-spec vals h-val b-val d-val)
  (setq w-type (if (assoc 'WALL-TYPE z-data) (cdr (assoc 'WALL-TYPE z-data)) "HPILE"))
  (if (= w-type "CIP")
    (progn
      (setq h-idx (atoi (if (assoc 'CIP-HPILE-IDX z-data) (cdr (assoc 'CIP-HPILE-IDX z-data)) "2")))
      (setq h-spec (nth (max 0 (min (1- (length *tsp-std-wall-list*)) h-idx)) *tsp-std-wall-list*))
      (setq vals (parse-h-spec h-spec))
      (setq h-val (if vals (nth 0 vals) 300))
      (setq d-val (atoi (if (assoc 'CIP-DIA z-data) (cdr (assoc 'CIP-DIA z-data)) "500")))
      (+ 1000000 (* h-val 1000) d-val)
    )
    (progn
      (setq h-spec (if (assoc 'WALL-SPEC z-data) (cdr (assoc 'WALL-SPEC z-data)) "H 300x300x10/15"))
      (setq vals (if (= h-spec "User-defined") (cdr (assoc 'WALL-CUSTOM z-data)) (parse-h-spec h-spec)))
      (if (null vals) (setq vals '(300 300 10 15)))
      (setq h-val (nth 0 vals) b-val (nth 1 vals))
      (+ (* h-val 1000) b-val)
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-resolve-wall-spans : 중첩 구간 발생 시 강성 우선 원칙으로 비중첩 벽체 스팬 리스트 생성
;;;;   - 반환: ((시작mm 끝mm 선택된_subzone_데이터) ...)
;;;;==========================================================================
(defun tsp-resolve-wall-spans (seg-data / seg-len sub-zones def-zones pts sz s-mm e-mm sorted-pts uniq-pts i a b mid-d best-sz best-score cur-score raw-spans merged-spans last-sp)
  (setq seg-len (cdr (assoc 'LENGTH seg-data)))
  (setq sub-zones (cdr (assoc 'SUB-ZONES seg-data)))
  (setq def-zones '())
  (foreach sz sub-zones
    (if (cdr (assoc 'IS-DEFINED sz))
      (setq def-zones (append def-zones (list sz)))
    )
  )
  (if (null def-zones)
    (list (list 0.0 seg-len seg-data))
    (progn
      (setq pts (list 0.0 seg-len))
      (foreach sz def-zones
        (setq s-mm (max 0.0 (min seg-len (* (cdr (assoc 'START-M sz)) 1000.0))))
        (setq e-mm (max 0.0 (min seg-len (* (cdr (assoc 'END-M sz)) 1000.0))))
        (setq pts (append pts (list s-mm e-mm)))
      )
      (setq sorted-pts (vl-sort pts '<))
      (setq uniq-pts '())
      (foreach p sorted-pts
        (if (or (null uniq-pts) (> (abs (- p (last uniq-pts))) 1.0))
          (setq uniq-pts (append uniq-pts (list p)))
        )
      )
      (setq raw-spans '() i 0)
      (while (< i (1- (length uniq-pts)))
        (setq a (nth i uniq-pts) b (nth (1+ i) uniq-pts))
        (setq mid-d (/ (+ a b) 2.0))
        (setq best-sz nil best-score -1)
        (foreach sz def-zones
          (setq s-mm (* (cdr (assoc 'START-M sz)) 1000.0))
          (setq e-mm (* (cdr (assoc 'END-M sz)) 1000.0))
          (if (and (<= (min s-mm e-mm) (+ mid-d 0.5)) (>= (max s-mm e-mm) (- mid-d 0.5)))
            (progn
              (setq cur-score (tsp-get-zone-wall-stiffness sz))
              (if (> cur-score best-score)
                (setq best-score cur-score best-sz sz)
              )
            )
          )
        )
        (if (null best-sz) (setq best-sz (car def-zones)))
        (setq raw-spans (append raw-spans (list (list a b best-sz))))
        (setq i (1+ i))
      )
      ;; 인접한 동일 구간 스팬 병합
      (setq merged-spans '())
      (foreach sp raw-spans
        (if (null merged-spans)
          (setq merged-spans (list sp))
          (progn
            (setq last-sp (last merged-spans))
            (if (equal (nth 2 last-sp) (nth 2 sp))
              (setq merged-spans (subst (list (nth 0 last-sp) (nth 1 sp) (nth 2 last-sp)) last-sp merged-spans))
              (setq merged-spans (append merged-spans (list sp)))
            )
          )
        )
      )
      merged-spans
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-expand-segments-for-wall-plan : SUB-ZONES가 있는 세그먼트를 벽체 스팬별 가상 세그먼트로 전개
;;;;==========================================================================
(defun tsp-expand-segments-for-wall-plan (seg-list / out-segs parent-map p-idx seg sub-zones spans sp-idx num-sp sp a b sz v-s v-a p1 p2 sub-seg k-pair new-id)
  (setq out-segs '() parent-map '() p-idx 0 new-id 1)
  (foreach seg seg-list
    (setq sub-zones (cdr (assoc 'SUB-ZONES seg)))
    (if (and sub-zones (> (length sub-zones) 1))
      (progn
        (setq spans (tsp-resolve-wall-spans seg))
        (setq num-sp (length spans) sp-idx 0)
        (setq v-s (cdr (assoc 'V-START seg)) v-a (cdr (assoc 'ANGLE seg)))
        (foreach sp spans
          (setq a (nth 0 sp) b (nth 1 sp) sz (nth 2 sp))
          (setq p1 (polar v-s v-a a))
          (setq p2 (polar v-s v-a b))
          (setq sub-seg seg)
          (foreach k-pair
            (list
              (cons 'ID new-id)
              (cons 'IS-DEFINED T)
              (cons 'WALL-TYPE (if (assoc 'WALL-TYPE sz) (cdr (assoc 'WALL-TYPE sz)) "HPILE"))
              (cons 'WALL-SPEC (if (assoc 'WALL-SPEC sz) (cdr (assoc 'WALL-SPEC sz)) (cdr (assoc 'WALL-SPEC seg))))
              (cons 'WALL-CUSTOM (if (assoc 'WALL-CUSTOM sz) (cdr (assoc 'WALL-CUSTOM sz)) (cdr (assoc 'WALL-CUSTOM seg))))
              (cons 'WALE-SPEC (if (assoc 'WALE-SPEC sz) (cdr (assoc 'WALE-SPEC sz)) (cdr (assoc 'WALE-SPEC seg))))
              (cons 'WALE-CUSTOM (if (assoc 'WALE-CUSTOM sz) (cdr (assoc 'WALE-CUSTOM sz)) (cdr (assoc 'WALE-CUSTOM seg))))
              (cons 'CTC (if (assoc 'CTC sz) (cdr (assoc 'CTC sz)) (cdr (assoc 'CTC seg))))
              (cons 'TIMBER-THICKNESS (if (assoc 'TIMBER-THICKNESS sz) (cdr (assoc 'TIMBER-THICKNESS sz)) 60))
              (cons 'CIP-HPILE-IDX (if (assoc 'CIP-HPILE-IDX sz) (cdr (assoc 'CIP-HPILE-IDX sz)) "2"))
              (cons 'CIP-WALE-IDX (if (assoc 'CIP-WALE-IDX sz) (cdr (assoc 'CIP-WALE-IDX sz)) "2"))
              (cons 'CIP-DIA (if (assoc 'CIP-DIA sz) (cdr (assoc 'CIP-DIA sz)) "500"))
              (cons 'CIP-MODE-IDX (if (assoc 'CIP-MODE-IDX sz) (cdr (assoc 'CIP-MODE-IDX sz)) "0"))
              (cons 'CIP-OVERLAP (if (assoc 'CIP-OVERLAP sz) (cdr (assoc 'CIP-OVERLAP sz)) "0"))
              (cons 'CIP-INTERVAL-IDX (if (assoc 'CIP-INTERVAL-IDX sz) (cdr (assoc 'CIP-INTERVAL-IDX sz)) "2"))
              (cons 'CIP-ANCHOR-N (if (assoc 'CIP-ANCHOR-N sz) (cdr (assoc 'CIP-ANCHOR-N sz)) "3"))
              (cons 'CIP-GROUTING-IDX (if (assoc 'CIP-GROUTING-IDX sz) (cdr (assoc 'CIP-GROUTING-IDX sz)) "0"))
              (cons 'CIP-GROUTING-SPACE-IDX (if (assoc 'CIP-GROUTING-SPACE-IDX sz) (cdr (assoc 'CIP-GROUTING-SPACE-IDX sz)) "0"))
              (cons 'CIP-GROUTING-SPACE-CUSTOM (if (assoc 'CIP-GROUTING-SPACE-CUSTOM sz) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM sz)) ""))
              (cons 'CIP-GROUTING-OFFSET-IDX (if (assoc 'CIP-GROUTING-OFFSET-IDX sz) (cdr (assoc 'CIP-GROUTING-OFFSET-IDX sz)) "0"))
              (cons 'CIP-GROUTING-OFFSET-CUSTOM (if (assoc 'CIP-GROUTING-OFFSET-CUSTOM sz) (cdr (assoc 'CIP-GROUTING-OFFSET-CUSTOM sz)) ""))
              (cons 'SUPPORT-LIST (if (assoc 'SUPPORT-LIST sz) (cdr (assoc 'SUPPORT-LIST sz)) (cdr (assoc 'SUPPORT-LIST seg))))
              (cons 'CORNER-BRACE (if (= sp-idx 0) (cdr (assoc 'CORNER-BRACE seg)) (list "N" nil)))
              (cons 'UPGRADE-WALE (if (= sp-idx 0) (cdr (assoc 'UPGRADE-WALE seg)) nil))
              (cons 'V-START p1)
              (cons 'V-END p2)
              (cons 'ANGLE v-a)
              (cons 'LENGTH (distance p1 p2))
            )
            (if (assoc (car k-pair) sub-seg)
              (setq sub-seg (subst k-pair (assoc (car k-pair) sub-seg) sub-seg))
              (setq sub-seg (append sub-seg (list k-pair)))
            )
          )
          (setq out-segs (append out-segs (list sub-seg)))
          (setq parent-map (append parent-map (list p-idx)))
          (setq sp-idx (1+ sp-idx) new-id (1+ new-id))
        )
      )
      (progn
        (setq seg (subst (cons 'ID new-id) (assoc 'ID seg) seg))
        (setq out-segs (append out-segs (list seg)))
        (setq parent-map (append parent-map (list p-idx)))
        (setq new-id (1+ new-id))
      )
    )
    (setq p-idx (1+ p-idx))
  )
  (list out-segs parent-map)
)

(princ "\nTSP.lsp 로드 완료!")
(princ "\n  명령어: TSP")
(princ)

;;;;==========================================================================
;;;; 디버깅 전용 실시간 파일 로그 출력 함수
;;;;==========================================================================
(defun tsp-log (msg / f path)
  (setq path "C:\\tsp_debug.log")
  (setq f (open path "a"))
  (if f
    (progn
      (write-line (strcat "[" (rtos (getvar "CDATE") 2 6) "] " msg) f)
      (close f)
    )
  )
  (princ (strcat "\n" msg))
  (princ)
)
