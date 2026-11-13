;;; ==========================================================================
;;; 파일명: TSP_5D_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part D)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-apply-wall-to-selected, tsp-is-brace-eligible-corner-p, tsp-apply-brace-to-selected, tsp-reset-segment-data, tsp-reset-all-segments 외 5개]
;;; 포함 함수: tsp-apply-wall-to-selected, tsp-is-brace-eligible-corner-p, tsp-apply-brace-to-selected, tsp-reset-segment-data, tsp-reset-all-segments, scale-dialog-callback, project-list-callback, main-dialog-callback, update-list-item, input-segment-wizard
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;==========================================================================
;;; defun tsp-apply-wall-to-selected : 선택된 세그먼트들에 흙막이벽 정보 복사
;;;==========================================================================
(defun tsp-apply-wall-to-selected (source-idx target-indices / source-data src-wall-type target-data new-item t-idx updated-list)
  (setq source-data (nth source-idx *segment-list*))
  (setq src-wall-type (if (assoc 'WALL-TYPE source-data) (cdr (assoc 'WALL-TYPE source-data)) "HPILE"))
  (setq updated-list '())
  (setq t-idx 0)
  (foreach target-data *segment-list*
    (if (and (member t-idx target-indices) (/= t-idx source-idx))
      (progn
        (setq new-item (list
          (cons 'ID (cdr (assoc 'ID target-data)))
          (cons 'NAME (cdr (assoc 'NAME target-data)))
          (cons 'IS-DEFINED T)
          (cons 'WALL-TYPE src-wall-type)
          (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW target-data)))
          (cons 'SOIL-DATA (cdr (assoc 'SOIL-DATA target-data)))
          (cons 'N-VALUES (cdr (assoc 'N-VALUES target-data)))
          (cons 'HOLE-NO (cdr (assoc 'HOLE-NO target-data)))

          ;; 흙막이벽 기본 변수군 (원본 데이터 덮어쓰기)
          (cons 'WALL-SPEC (cdr (assoc 'WALL-SPEC source-data)))
          (cons 'WALL-CUSTOM (cdr (assoc 'WALL-CUSTOM source-data)))
          (cons 'WALE-SPEC (cdr (assoc 'WALE-SPEC source-data)))
          (cons 'WALE-CUSTOM (cdr (assoc 'WALE-CUSTOM source-data)))
          (cons 'CTC (cdr (assoc 'CTC source-data)))
          (cons 'TIMBER-THICKNESS (cdr (assoc 'TIMBER-THICKNESS source-data)))
          (cons 'MAX-DEPTH (cdr (assoc 'MAX-DEPTH source-data)))
          (cons 'EMBED-DEPTH (cdr (assoc 'EMBED-DEPTH source-data)))
          (cons 'ROCK-EMBED (cdr (assoc 'ROCK-EMBED source-data)))

          ;; CIP 전용 변수군 (원본이 CIP일 때만 복사, 아니면 타겟 데이터 유지)
          (cons 'CIP-MAX-DEPTH (if (= src-wall-type "CIP") (if (assoc 'CIP-MAX-DEPTH source-data) (cdr (assoc 'CIP-MAX-DEPTH source-data)) "10.0") (cdr (assoc 'CIP-MAX-DEPTH target-data))))
          (cons 'CIP-EMBED-DEPTH (if (= src-wall-type "CIP") (if (assoc 'CIP-EMBED-DEPTH source-data) (cdr (assoc 'CIP-EMBED-DEPTH source-data)) "3.0") (cdr (assoc 'CIP-EMBED-DEPTH target-data))))
          (cons 'CIP-HPILE-IDX (if (= src-wall-type "CIP") (if (assoc 'CIP-HPILE-IDX source-data) (cdr (assoc 'CIP-HPILE-IDX source-data)) "2") (cdr (assoc 'CIP-HPILE-IDX target-data))))
          (cons 'CIP-WALE-IDX (if (= src-wall-type "CIP") (if (assoc 'CIP-WALE-IDX source-data) (cdr (assoc 'CIP-WALE-IDX source-data)) "2") (cdr (assoc 'CIP-WALE-IDX target-data))))
          (cons 'CIP-DIA (if (= src-wall-type "CIP") (if (assoc 'CIP-DIA source-data) (cdr (assoc 'CIP-DIA source-data)) "500") (cdr (assoc 'CIP-DIA target-data))))
          (cons 'CIP-MODE-IDX (if (= src-wall-type "CIP") (if (assoc 'CIP-MODE-IDX source-data) (cdr (assoc 'CIP-MODE-IDX source-data)) "0") (cdr (assoc 'CIP-MODE-IDX target-data))))
          (cons 'CIP-OVERLAP (if (= src-wall-type "CIP") (if (assoc 'CIP-OVERLAP source-data) (cdr (assoc 'CIP-OVERLAP source-data)) "0") (cdr (assoc 'CIP-OVERLAP target-data))))
          (cons 'CIP-EQUAL-DIVIDE (if (= src-wall-type "CIP") (if (assoc 'CIP-EQUAL-DIVIDE source-data) (cdr (assoc 'CIP-EQUAL-DIVIDE source-data)) "1") (cdr (assoc 'CIP-EQUAL-DIVIDE target-data))))
          (cons 'CIP-INTERVAL-IDX (if (= src-wall-type "CIP") (if (assoc 'CIP-INTERVAL-IDX source-data) (cdr (assoc 'CIP-INTERVAL-IDX source-data)) "2") (cdr (assoc 'CIP-INTERVAL-IDX target-data))))
          (cons 'CIP-ANCHOR-N (if (= src-wall-type "CIP") (if (assoc 'CIP-ANCHOR-N source-data) (cdr (assoc 'CIP-ANCHOR-N source-data)) "1") (cdr (assoc 'CIP-ANCHOR-N target-data))))
          ;; 차수그라우팅 변수군 (CIP 및 H-Pile+토류판 공통 복사)
          (cons 'CIP-GROUTING-IDX (if (assoc 'CIP-GROUTING-IDX source-data) (cdr (assoc 'CIP-GROUTING-IDX source-data)) "0"))
          (cons 'CIP-GROUTING-SPACE-IDX (if (assoc 'CIP-GROUTING-SPACE-IDX source-data) (cdr (assoc 'CIP-GROUTING-SPACE-IDX source-data)) "0"))
          (cons 'CIP-GROUTING-SPACE-CUSTOM (if (assoc 'CIP-GROUTING-SPACE-CUSTOM source-data) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM source-data)) ""))
          (cons 'CIP-GROUTING-OFFSET-IDX (if (assoc 'CIP-GROUTING-OFFSET-IDX source-data) (cdr (assoc 'CIP-GROUTING-OFFSET-IDX source-data)) "0"))
          (cons 'CIP-GROUTING-OFFSET-CUSTOM (if (assoc 'CIP-GROUTING-OFFSET-CUSTOM source-data) (cdr (assoc 'CIP-GROUTING-OFFSET-CUSTOM source-data)) ""))

          (cons 'WATER-CHK (cdr (assoc 'WATER-CHK target-data)))
          (cons 'WATER-DEPTH (cdr (assoc 'WATER-DEPTH target-data)))
          (cons 'SHOTCRETE-ENABLE (cdr (assoc 'SHOTCRETE-ENABLE target-data)))
          (cons 'SHOTCRETE-CHK (cdr (assoc 'SHOTCRETE-CHK target-data)))
          (cons 'SHOTCRETE-LAYER (cdr (assoc 'SHOTCRETE-LAYER target-data)))
          (cons 'SHOTCRETE-THICK (cdr (assoc 'SHOTCRETE-THICK target-data)))
          (cons 'SUPPORT-LIST (cdr (assoc 'SUPPORT-LIST target-data)))
          (cons 'CORNER-BRACE (cdr (assoc 'CORNER-BRACE target-data)))
          (cons 'UPGRADE-WALE (cdr (assoc 'UPGRADE-WALE target-data)))
          (cons 'V-START (cdr (assoc 'V-START target-data)))
          (cons 'V-END (cdr (assoc 'V-END target-data)))
          (cons 'ANGLE (cdr (assoc 'ANGLE target-data)))
          (cons 'LENGTH (cdr (assoc 'LENGTH target-data)))
        ))
        (setq updated-list (append updated-list (list new-item)))
      )
      (setq updated-list (append updated-list (list target-data)))
    )
    (setq t-idx (1+ t-idx))
  )
  (setq *segment-list* updated-list)
  (setq *tsp-data-dirty* T)
)

;;;;==========================================================================
;;;; defun tsp-is-brace-eligible-corner-p : 해당 세그먼트 시작 코너(idx)가 기하학적으로 사보강재 설치 가능한 볼록 코너인지 판별
;;;;==========================================================================
(defun tsp-is-brace-eligible-corner-p (idx / num-segs is-closed orient curr-seg prev-seg pt0 pt1 pt2 is-convex ang-v1 ang-v2 diff seg-len prev-seg-len min-len)
  (setq num-segs (length *segment-list*))
  (if (or (< idx 0) (>= idx num-segs))
    nil
    (progn
      (setq is-closed
        (if (and (boundp '*tsp-boundary-ent*) *tsp-boundary-ent*)
          (is-closed-polyline *tsp-boundary-ent*)
          (if (> num-segs 1)
            (equal (cdr (assoc 'V-START (nth 0 *segment-list*)))
                   (cdr (assoc 'V-END (last *segment-list*)))
                   0.1)
            nil
          )
        )
      )
      (setq orient
        (cond
          ((and (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*) *tsp-boundary-orient*)
          ((and (boundp '*tsp-imported-orient*) *tsp-imported-orient*) *tsp-imported-orient*)
          (t 1)
        )
      )
      (setq curr-seg (nth idx *segment-list*))
      (setq pt1 (cdr (assoc 'V-START curr-seg)))
      (setq pt2 (cdr (assoc 'V-END curr-seg)))
      (setq prev-seg
        (if is-closed
          (nth (if (= idx 0) (1- num-segs) (1- idx)) *segment-list*)
          (if (> idx 0) (nth (1- idx) *segment-list*) nil)
        )
      )
      (setq pt0 (if prev-seg (cdr (assoc 'V-START prev-seg)) nil))
      (setq is-convex nil)
      (if (and pt0 pt1 pt2)
        (progn
          (if (is-corner-convex pt0 pt1 pt2 orient) (setq is-convex T))
          (setq ang-v1 (angle pt1 pt0))
          (setq ang-v2 (angle pt1 pt2))
          (setq diff (abs (- ang-v1 ang-v2)))
          (if (> diff pi) (setq diff (- (* 2.0 pi) diff)))
          (if (>= diff (* 160.0 (/ pi 180.0)))
            (setq is-convex nil)
          )
          (setq seg-len (distance pt1 pt2))
          (setq prev-seg-len (distance pt0 pt1))
          (setq min-len (min seg-len prev-seg-len))
          (if (< min-len 1000.0)
            (setq is-convex nil)
          )
        )
      )
      is-convex
    )
  )
)

;;;==========================================================================
;;; defun tsp-apply-brace-to-selected : 선택된 세그먼트들에 사보강재 정보 복사 (기하학적 유효 코너 자동 복구 및 결과 반환)
;;;==========================================================================
(defun tsp-apply-brace-to-selected (source-idx target-indices / source-data src-brace src-upg target-data new-item t-idx updated-list tgt-brace can-brace copied-count skipped-names)
  (setq source-data (nth source-idx *segment-list*))
  (setq src-brace (cdr (assoc 'CORNER-BRACE source-data)))
  (setq src-upg (cdr (assoc 'UPGRADE-WALE source-data)))
  (setq updated-list '())
  (setq copied-count 0)
  (setq skipped-names '())
  (setq t-idx 0)
  (foreach target-data *segment-list*
    (if (and (member t-idx target-indices) (/= t-idx source-idx))
      (progn
        (setq tgt-brace (cdr (assoc 'CORNER-BRACE target-data)))
        ;; 기존에 "Y"이거나, 과거 status 6 등에 의해 ("N" nil)로 덮어씌워졌더라도 기하학적으로 유효한 볼록 코너이면 복사 허용
        (setq can-brace (or (and tgt-brace (= (car tgt-brace) "Y"))
                            (tsp-is-brace-eligible-corner-p t-idx)))
        (if can-brace
          (progn
            (setq new-item target-data)
            (setq new-item (subst (cons 'CORNER-BRACE src-brace) (assoc 'CORNER-BRACE new-item) new-item))
            (if (assoc 'UPGRADE-WALE new-item)
              (setq new-item (subst (cons 'UPGRADE-WALE src-upg) (assoc 'UPGRADE-WALE new-item) new-item))
              (setq new-item (append new-item (list (cons 'UPGRADE-WALE src-upg))))
            )
            (setq copied-count (1+ copied-count))
            (setq updated-list (append updated-list (list new-item)))
          )
          (progn
            ;; 사보강재 설치 불가 코너(개방형 시점 등)는 IS-DEFINED 및 기존 데이터를 건드리지 않고 그대로 유지
            (setq skipped-names (append skipped-names (list (cdr (assoc 'NAME target-data)))))
            (setq updated-list (append updated-list (list target-data)))
          )
        )
      )
      (setq updated-list (append updated-list (list target-data)))
    )
    (setq t-idx (1+ t-idx))
  )
  (setq *segment-list* updated-list)
  (if (> copied-count 0) (setq *tsp-data-dirty* T))
  (list copied-count skipped-names)
)

;;;;==========================================================================
;;;; defun tsp-reset-segment-data : 특정 세그먼트의 설정을 초기화
;;;;==========================================================================
(defun tsp-reset-segment-data (idx / old-data new-data old-brace reset-brace)
  (setq old-data (nth idx *segment-list*))
  (setq old-brace (cdr (assoc 'CORNER-BRACE old-data)))
  ;; 과거 ("N" nil)로 덮어씌워진 볼록 코너라도 기하학적 판별을 통해 원래의 "Y" 기본값으로 복원
  (setq reset-brace
    (if (or (tsp-is-brace-eligible-corner-p idx) (and old-brace (= (car old-brace) "Y")))
      (list "Y" nil "1" "0" "0.0" "1" "50")
      (list "N" nil)
    )
  )

  (setq new-data (list
    (cons 'ID (cdr (assoc 'ID old-data)))
    (cons 'NAME (strcat "Seg-" (itoa (1+ idx))))
    (cons 'IS-DEFINED nil)
    (cons 'SECTION-DRAW nil)
    (cons 'SOIL-DATA nil)
    (cons 'N-VALUES nil)
    (cons 'HOLE-NO "BH-")
    (cons 'WALL-SPEC *tsp-hpile-spec*)
    (cons 'WALL-CUSTOM '(300 300 10 15))
    (cons 'WALE-SPEC *tsp-wale-spec*)
    (cons 'WALE-CUSTOM '(300 300 10 15))
    (cons 'CTC 2.0)
    (cons 'TIMBER-THICKNESS 60)
    (cons 'MAX-DEPTH 10.0)
    (cons 'EMBED-DEPTH 3.0)
    (cons 'WATER-CHK "0")
    (cons 'WATER-DEPTH 0.0)
    (cons 'SHOTCRETE-ENABLE "1")
    (cons 'SHOTCRETE-CHK "0")
    (cons 'SHOTCRETE-LAYER "")
    (cons 'SHOTCRETE-THICK 0.0)
    (cons 'SUPPORT-LIST '())
    (cons 'CORNER-BRACE reset-brace)
    (cons 'UPGRADE-WALE nil)
    (cons 'V-START (cdr (assoc 'V-START old-data)))
    (cons 'V-END (cdr (assoc 'V-END old-data)))
    (cons 'ANGLE (cdr (assoc 'ANGLE old-data)))
    (cons 'LENGTH (cdr (assoc 'LENGTH old-data)))
  ))
  (setq *segment-list* (update-list-item *segment-list* idx new-data))
  (setq *tsp-data-dirty* T)
)

;;;;==========================================================================
;;;; defun tsp-reset-all-segments : 모든 세그먼트의 설정을 일괄 초기화
;;;;==========================================================================
(defun tsp-reset-all-segments (/ i)
  (setq i 0)
  (repeat (length *segment-list*)
    (tsp-reset-segment-data i)
    (setq i (1+ i))
  )
)

;;;;==========================================================================
;;;; defun scale-dialog-callback : 스케일 설정 대화상자 콜백
;;;;==========================================================================
(defun scale-dialog-callback (dcl-path / dcl-id result s-sec s-pln)
  (setq dcl-id (load_dialog dcl-path))
  (if (new_dialog "tsp_scale" dcl-id)
    (progn
      (set_tile "scale_section" (itoa *tsp-scale-section*))
      (set_tile "scale_plan" (itoa *tsp-scale-plan*))
      (action_tile "accept"
        "(progn
           (setq s-sec (get_tile \"scale_section\"))
           (setq s-pln (get_tile \"scale_plan\"))
           (if (and (is-numeric s-sec) (is-numeric s-pln))
             (progn
               (setq *tsp-scale-section* (atoi s-sec))
               (setq *tsp-scale-plan* (atoi s-pln))
               (done_dialog 1))
             (alert \"스케일은 숫자를 입력해주세요.\")
           )
         )"
      )
      (action_tile "back" "(done_dialog 4)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      result
    )
  )
)

;;;;==========================================================================
;;;; defun project-list-callback : 프로젝트 불러오기 및 삭제 목록 팝업
;;;;==========================================================================
(defun project-list-callback (dcl-path / dcl-id result proj-list sel-idx refresh-proj-list)
  (setq dcl-id (load_dialog dcl-path))
  (setq proj-list (tsp-get-project-list))
  (if (null proj-list)
    (progn (alert "저장된 프로젝트가 없습니다.") (unload_dialog dcl-id) nil)
    (if (new_dialog "tsp_load_list" dcl-id)
      (progn
        ;; 내부 함수: 목록 새로고침

;;;;==========================================================================
;;;; defun refresh-proj-list : 자동 정리된 함수
;;;;==========================================================================
        (defun refresh-proj-list ()
          (setq proj-list (tsp-get-project-list))
          (start_list "proj_list")
          (mapcar 'add_list proj-list)
          (end_list)
        )
        (refresh-proj-list)
        (action_tile "accept"
          "(progn
             (setq sel-idx (atoi (get_tile \"proj_list\")))
             (if (and (>= sel-idx 0) (< sel-idx (length proj-list)))
               (if (tsp-load-project-data (nth sel-idx proj-list))
                 (done_dialog 1)
                 (done_dialog 0)
               )
               (alert \"불러올 목록을 선택해주세요.\")
             )
           )"
        )
        (action_tile "btn_del_proj"
          "(progn
             (setq sel-idx (atoi (get_tile \"proj_list\")))
             (if (and (>= sel-idx 0) (< sel-idx (length proj-list)))
               (progn
                 (tsp-delete-project-data (nth sel-idx proj-list))
                 (refresh-proj-list) ;; 삭제 후 목록 리프레쉬
               )
               (alert \"삭제할 목록을 선택해주세요.\")
             )
           )"
        )
        (action_tile "cancel" "(done_dialog 0)")
        (setq result (start_dialog))
        (unload_dialog dcl-id)
        (= result 1) ; 로드 성공 여부 반환 (T/nil)
      )
    )
  )
)

;;;;==========================================================================
;;;; defun main-dialog-callback : 메인 선택 대화상자 콜백
;;;;==========================================================================
(defun main-dialog-callback (dcl-path / dcl-id result)
  (setq dcl-id (load_dialog dcl-path))
  (if (new_dialog "tsp_main" dcl-id)
    (progn
      ;; [3단계] H-Pile: 즉시 "HPILE" 확정
      (action_tile "btn_hpile" "(progn (setq *tsp-wall-type* \"HPILE\") (done_dialog 1))")
      ;; [3단계] C.I.P: wall-type 건드리지 않고 step만 넘김 (확정은 CIP 다이얼로그 OK에서만)
      (action_tile "btn_cip" "(done_dialog 11)")
      (action_tile "btn_scw" "(alert \"SCW 공법은 아직 구현되지 않았습니다.\")")
      (action_tile "back" "(done_dialog 2)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      result
    )
  )
)

;;;;==========================================================================
;;;; defun update-list-item : 리스트의 특정 인덱스(idx) 요소를 new-val로 안전하게 교체
;;;;==========================================================================
(defun update-list-item (lst idx new-val / i new-list)
  (setq i 0 new-list '())
  (foreach item lst
    (if (= i idx)
      (setq new-list (append new-list (list new-val)))
      (setq new-list (append new-list (list item)))
    )
    (setq i (1+ i))
  )
  new-list
)

;;;;==========================================================================
;;;; defun input-segment-wizard : [입력 모드] 단계별 마법사 방식
;;;;==========================================================================
(defun input-segment-wizard (dcl-path idx / seg-data current-draw current-name sub-status step input-name dcl-id b-data max-limit new-brace temp-seg-data limits res cur-idx brace-loop found-valid search-idx loop-count max-loop check-b)
  (load-segment-to-globals idx)
  (setq seg-data (nth idx *segment-list*))
  (if (not (cdr (assoc 'IS-DEFINED seg-data)))
    (setq *tsp-wall-type* "HPILE")
  )
  (setq current-draw (cdr (assoc 'SECTION-DRAW seg-data)))
  (if (and (not (cdr (assoc 'IS-DEFINED seg-data)))
           *tsp-soil-layers*
           (/= *tsp-soil-layers* '()))
    (setq current-draw T)
  )
  (setq current-name (cdr (assoc 'NAME seg-data)))
  (setq b-data (cdr (assoc 'CORNER-BRACE seg-data)))
  (if (and (or (null b-data) (= (car b-data) "N")) (tsp-is-brace-eligible-corner-p idx))
    (progn
      (setq b-data (list "Y" nil "1" "0" "0.0" "1" "50"))
      (setq seg-data (subst (cons 'CORNER-BRACE b-data) (assoc 'CORNER-BRACE seg-data) seg-data))
      (setq *segment-list* (update-list-item *segment-list* idx seg-data))
    )
  )
  (setq step 1)
  (while (and (>= step 1) (or (< step 7) (= step 41)))
    (setq sub-status 0)
    (cond
      ((= step 1)        (setq dcl-id (load_dialog dcl-path))
        (if (new_dialog "tsp_section_mode" dcl-id)
          (progn
            (set_tile "dlg_title" (strcat current-name " 입력 (단계별)"))
            (set_tile "seg_name" current-name)
            (set_tile "chk_section" (if current-draw "1" "0"))
            (action_tile "accept" "(setq input-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 1)")
            (action_tile "cancel" "(done_dialog 0)")
            (setq sub-status (start_dialog))
          )
        )
        (unload_dialog dcl-id)
        (if (= sub-status 1)
          (progn
            (if (and input-name (/= input-name "")) (setq current-name input-name))
            (if current-draw (setq step 2) (setq step 3))
          )
          (setq step -1)
        )
      )
      ((= step 2) (setq sub-status (soil-dialog-callback dcl-path)) (if (= sub-status 1) (setq step 3) (if (= sub-status 2) (setq step 1) (setq step -1))))
      ((= step 3)
        (setq sub-status (main-dialog-callback dcl-path))
        (cond
          ((= sub-status 1) (setq step 4))
          ((= sub-status 11) (setq step 41))
          ((= sub-status 2) (if current-draw (setq step 2) (setq step 1)))
          (t (setq step -1))
        )
      )
      ((= step 4) (setq sub-status (hpile-dialog-callback dcl-path)) (if (= sub-status 1) (setq step 5) (if (= sub-status 2) (setq step 3) (setq step -1))))
      ((= step 41) (setq sub-status (tsp-run-cip-dialog)) (if (= sub-status 1) (setq step 5) (if (= sub-status 2) (setq step 3) (setq step -1))))
      ((= step 5)
       (setq sub-status (support-dialog-callback dcl-path))
       (if (= sub-status 1)
         (if (and b-data (= (car b-data) "Y"))
           (setq step 6)
           (setq step 7))
         (if (= sub-status 2)
           (if (= *tsp-wall-type* "CIP") (setq step 41) (setq step 4))
           (setq step -1)
         )
       )
      )
      ((= step 6)
       (setq cur-idx idx)
       (setq brace-loop T)
       (while brace-loop
         (setq temp-seg-data (nth cur-idx *segment-list*))
         (setq limits (tsp-calculate-brace-limit cur-idx))
         (setq res (brace-dialog-callback dcl-path temp-seg-data limits 0))
         (if (and res (listp res))
           (progn
             (setq old-b-list (if (and (cdr (assoc 'CORNER-BRACE temp-seg-data)) (cadr (cdr (assoc 'CORNER-BRACE temp-seg-data)))) (cadr (cdr (assoc 'CORNER-BRACE temp-seg-data))) nil))
             (setq new-b-list (if (and (cadr res) (cadr (cadr res))) (cadr (cadr res)) nil))
             (setq is-dirty (not (and (equal old-b-list new-b-list) (equal (cdr (assoc 'UPGRADE-WALE temp-seg-data)) (caddr res)))))

             (setq b-data (cadr res))
             (setq temp-seg-data (subst (cons 'CORNER-BRACE b-data) (assoc 'CORNER-BRACE temp-seg-data) temp-seg-data))
             (setq temp-seg-data (subst (cons 'UPGRADE-WALE (caddr res)) (assoc 'UPGRADE-WALE temp-seg-data) temp-seg-data))
             (setq *segment-list* (update-list-item *segment-list* cur-idx temp-seg-data))

             ;; 변경사항이 있을 때만 재생성
             (if (and is-dirty *tsp-boundary-ent* *tsp-boundary-orient*)
               (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
             )
             (if (= (car res) "NEXT")
               (progn
                 (setq found-valid nil search-idx cur-idx loop-count 0 max-loop (length *segment-list*))
                 (while (and (not found-valid) (< loop-count max-loop))
                   (setq search-idx (1+ search-idx))
                   (if (>= search-idx max-loop)
                     (if (is-closed-polyline *tsp-boundary-ent*)
                       (setq search-idx 0)
                       (setq search-idx 9999)
                     )
                   )
                   (if (< search-idx max-loop)
                     (progn
                       (setq check-b (cdr (assoc 'CORNER-BRACE (nth search-idx *segment-list*))))
                       (if (not (and check-b (= (car check-b) "N")))
                         (setq found-valid T)
                       )
                     )
                   )
                   (setq loop-count (1+ loop-count))
                 )
                 (if found-valid
                   (setq cur-idx search-idx)
                   (progn
                     (alert "더 이상 사보강재(Corner Strut)를 입력할 유효한 모서리가 없습니다.\n입력을 완료합니다.")
                     (setq brace-loop nil)
                     (setq step 7) ; 더 이상 다음이 없으면 7단계로 이동
                   )
                 )
               )
               (progn
                 (setq brace-loop nil)
                 (setq step 7) ; COMP (완료)를 누르면 정상적으로 7단계로 이동
               )
             )
           )
           (progn
             (setq brace-loop nil)
             (setq step -1) ; 취소 시 마법사 종료
           )
         )
       )
      )
    )
  )
  (if (= step 7)
    (progn
      (setq seg-data (nth idx *segment-list*))
      (setq seg-data (subst (cons 'NAME current-name) (assoc 'NAME seg-data) seg-data))
      (setq seg-data (subst (cons 'SECTION-DRAW current-draw) (assoc 'SECTION-DRAW seg-data) seg-data))
      (setq *segment-list* (update-list-item *segment-list* idx seg-data))
      (sync-globals-to-segment idx)
      (setq *tsp-data-dirty* T)
    )
  )
)
