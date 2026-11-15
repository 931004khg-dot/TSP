;;; ==========================================================================
;;; TSP.lsp - Temporary Structure Plan (Integrated Modular Loader & Index)
;;; ==========================================================================
;;;
;;; [AI 협업 핵심 지침 - 새 채팅창에서 작업을 시작할 때 필독]
;;; --------------------------------------------------------------------------
;;; 1. 이 파일(TSP.lsp)은 전체 시스템을 총괄하는 '메인 로더(Loader) 겸 함수 사전'입니다.
;;; 2. 사용자가 기능 추가, 오류 수정, 또는 검토를 요청하면 AI는 절대 전체 코드를
;;;    달라고 하지 말고, 아래 [모듈별 역할 및 포함 함수 사전]과 [파일 간 의존성 맵]을
;;;    먼저 분석하여 수정이 필요한 '특정 하위 모듈 파일(1~2개)'만 공유해 달라고
;;;    사용자에게 요청해야 합니다.
;;; 3. 코드 수정 시에는 교체할 위치와 내용을 텍스트 블록으로 나누어 답변하지 말고,
;;;    반드시 파이썬 자동 패치 스크립트(apply_patch.py) 전체 코드를 제공해야 합니다.
;;;    [apply_patch.py 작성 시 AI가 반드시 지켜야 할 5대 필수 요령]
;;;    (1) 더블클릭 실행 시 창 꺼짐 절대 방지 (대기 코드 및 예외 처리 필수):
;;;        - CMD를 수동으로 열지 않고 더블클릭만으로 실행해도 창이 바로 꺼지지 않도록,
;;;          메인 실행부를 `try ... except Exception:`으로 감싸고 실패 시 `traceback.print_exc()`로
;;;          정확히 몇 번째 줄에서 왜 실패했는지(Traceback) 상세히 출력해야 합니다.
;;;        - `finally:` 블록에 반드시 `input("결과를 확인하셨으면 엔터(Enter) 키를 누르세요...")`
;;;          대기 코드를 넣어 사용자가 로그를 끝까지 확인할 수 있게 해야 합니다.
;;;    (2) 다중 폴더 자동 파일 탐색 (FileNotFoundError 원천 차단):
;;;        - 반드시 `os.path.dirname(os.path.abspath(__file__))`을 기준으로 스크립트가 위치한 폴더,
;;;          상위 폴더(..), 그리고 하위 폴더(os.walk)를 자동으로 탐색하여 대상 파일(대소문자 무관)을
;;;          찾는 함수를 포함해야 합니다.
;;;    (3) 스마트 정밀 검증 (Safe Replace - 공백 정규화 및 유일성 검사):
;;;        - 줄바꿈(\r\n, \n)과 연속된 공백, 탭 등을 단일 공백으로 정규화(Normalize)하여 비교하되,
;;;          교체 대상인 '기존 원본 코드(old_block)'가 파일 내에 정확히 1곳만 존재하는지 사전 검사합니다.
;;;        - 검색 결과가 0곳이면 이미 새 코드(new_block)가 적용되었는지 확인하여 안내하고,
;;;          그렇지 않거나 2곳 이상(중복 매칭)이면 원본을 일절 건드리지 않고 에러 메시지를 띄우며 즉시 중단합니다.
;;;    (4) 자동 백업(.bak) 및 인코딩 보호:
;;;        - 파일 수정 전 반드시 `.bak` 백업본을 자동 생성해야 합니다.
;;;        - 파이썬(.py) 파일은 UTF-8을 유지하고, 리습(.lsp) 파일은 ANSI(CP949)로 인코딩해도
;;;          문제가 없도록 유니코드 특수 공백 문자를 일반 공백(' ')으로 정화하여 저장해야 합니다.
;;;    (5) 코드 축약 절대 금지 및 주석 포함:
;;;        - 패치 스크립트 변수에 담기는 기존 코드(old_block) 및 새 코드(new_block)는
;;;          `;; [내부 함수 생략]` 등의 형태로 절대 축약하지 말고 완벽한 원문 그대로 작성하며,
;;;          코드(함수) 생성 시 반드시 설명 주석을 포함해야 합니다.
;;;
;;; ==========================================================================
;;; [향후 기능 추가로 코드가 길어질 경우의 관리 매뉴얼 (확장 & 재분할 수칙)]
;;; --------------------------------------------------------------------------
;;; * 수칙 1 (파일 크기 임계치 관리 - 유효 800줄 / 30~35KB 기준):
;;;   - 단일 하위 모듈 파일이 기능 추가로 인해 약 800~900줄(또는 35KB)을 넘어가면,
;;;     채팅창 업로드 시 코드 뒷부분이 잘릴 위험이 발생합니다.
;;;   - AI는 공유받은 모듈 파일의 길이가 800줄에 육박하는 것을 감지하면,
;;;     작업을 마친 후 사용자에게 "해당 모듈 파일이 커지고 있으니 2개의 하위 파일로
;;;     안전하게 분할하는 패치 스크립트를 제공할까요?"라고 먼저 제안해야 합니다.
;;;
;;; * 수칙 2 (신규 대규모 기능 추가 시 독립 모듈화):
;;;   - 새로운 벽체 공법(예: SCW, 지하연속벽), 모서리 단면도 작도, 수량산출서 집계 등
;;;     독립된 신규 기능을 추가할 때는 기존 파일을 비대하게 만들지 말고
;;;     새로운 모듈 파일(예: "TSP_7_Corner_Section.lsp")로 분리하여 작성합니다.
;;;   - 새 파일을 만든 후에는 이 메인 파일(TSP.lsp) 하단의 `*tsp-module-files*` 리스트에
;;;     새 파일 이름만 한 줄 추가하고, 아래 함수 사전에 함수 목록만 기입하면 즉시 연동됩니다.
;;;
;;; * 수칙 3 (한 파일을 수정할 때 다른 파일에 이상이 생기는지 확인하는 방법):
;;;   - AutoLISP은 모든 모듈이 같은 메모리 공간(전역 변수 및 함수 심볼)을 공유합니다.
;;;   - 따라서 어떤 모듈의 함수를 수정할 때는 아래 [파일 간 의존성 및 충돌 방지 체크리스트]를
;;;     반드시 먼저 대조하여, 함수의 '입력 인자 개수/순서', '반환값 형태', '전역 변수 구조'가
;;;     바뀌지 않는지 사전 검증(Validation)해야 합니다.
;;;
;;; * 수칙 4 (파이썬 GUI 연동 프로그램 존재 주의):
;;;   - 이 LISP 시스템은 외부 파이썬 GUI 프로그램(`geox_to_tsp_gui.py`)과 CSV 파일 및
;;;     다이렉트 통신(예: `tsp-direct-plan-from-python`)으로 데이터를 긴밀하게 주고받습니다.
;;;   - AI는 `*segment-list*`의 assoc 키 이름, `SUPPORT-LIST` 및 `CORNER-BRACE`의
;;;     리스트 내 항목 개수나 인덱스 순서를 변경해야 할 경우 등, 반드시 파이썬 파일의
;;;     파싱/문자열 생성 로직도 일치하도록 수정해야 통신 에러가 발생하지 않음을 인지해야 합니다.
;;;   - 연동프로그램과 관련된 구조를 변경할 때는 사용자에게 "파이썬 연동 파일(geox_to_tsp_gui.py)
;;;     도 함께 수정해야 하니 해당 코드를 공유해 달라"고 선제적으로 요청해야 합니다.
;;;   - 이때 파이썬 파일(`geox_to_tsp_gui.py`) 역시 전체 코드를 출력하지 말고, 
;;;     `apply_patch.py`를 통해 교체가 필요한 특정 함수나 클래스 블록만 정확히
;;;     찾아내어 덮어쓰도록 패치 스크립트를 작성해 주어야 합니다.
;;; ==========================================================================
;;; [파일 간 의존성 및 충돌 방지 체크리스트 (Cross-Module Dependency Map)]
;;; --------------------------------------------------------------------------
;;; 1. 핵심 전역 리스트 `*segment-list*` (연관 배열 구조):
;;;    - 생성/초기화 : Geometry 모듈 (`init-segment-data`)
;;;    - 저장/불러오기: Globals_Utils 모듈 (`tsp-save-project-data`, `tsp-load-project-data`)
;;;    - UI 동기화   : DCL 모듈 (`sync-globals-to-segment`, `load-segment-to-globals`)
;;;    - CSV 입출력  : Main_CSV 모듈 (`tsp-write-project-csv`, `tsp-parse-csv-file-data`)
;;;    => 주의: 세그먼트 assoc 키(Key)를 추가/변경할 때는 위 4개 구간의 호환성을 확인하세요.
;;;
;;; 2. 지보재 데이터 `SUPPORT-LIST` (버팀보 14개 요소 / 앵커 12개 요소):
;;;    - 입력/편집   : DCL/Strut 모듈 (`strut-input-dialog`, `anchor-input-dialog`)
;;;    - 단면 작도   : Section 모듈 (`draw-section-hpile`, `draw-section-cip`)
;;;    - 평면 작도   : Plan 모듈 (앵커/띠장/CIP 앵커), DCL/Strut 모듈 (`tsp-draw-manual-strut`)
;;;    => 주의: 지보재 리스트의 nth 인덱스 순서를 절대 임의로 변경하지 마세요.
;;;
;;; 3. 여러 모듈에서 공동 호출하는 핵심 공용 함수 (시그니처 변경 금지):
;;;    - `create-layer-if-not-exists`, `parse-h-spec` (Globals_Utils -> 전 모듈 호출)
;;;    - `extract-vertices`, `is-closed-polyline`, `is-corner-convex` (Geometry -> 전 모듈 호출)
;;;    - `create-hpile-section-block`, `create-post-pile-block` (Plan -> Section, Strut 공동 호출)
;;;    - `create-strut-jack-block`, `create-strut-screw-jack-block` (Section -> Plan, Strut 공동 호출)
;;;    - `tsp-draw-wedge-connection`, `tsp-draw-closed-poly` (Plan -> Strut 까치발/비평행띠장 공동 호출)
;;;    - `tsp-redraw-realtime-plan` (Main_CSV -> DCL/Strut 실시간 미리보기에서 호출)
;;; ==========================================================================
;;; ==========================================================================
;;; [모듈별 역할 및 포함 함수 사전 (총 44개 하위 모듈)]
;;; ==========================================================================
;;;
;;; ▶ [TSP_1A_Globals_Utils.lsp] (546줄 / 25.0KB) - 전역 변수 및 공통/그룹 유틸리티 (Part A)
;;;    역할: 해치 패턴(16종), 표준 H형강 규격, 전역 설정 변수, 레이어/해치 유틸리티, 프로젝트 저장/불러오기/자동매칭, 도면 그룹(ACAD_GROUP) 제어 [get-hatch-scale-factor, create-layer-if-not-exists, is-numeric, parse-h-spec, format-depth-text 외 5개]
;;;    포함 함수 (10개):
;;;      - get-hatch-scale-factor : 도면의 MEASUREMENT 변수(0=Imperial, 1=Metric)에 따라 해치 스케일 보정값 반환
;;;      - create-layer-if-not-exists : 레이어가 존재하지 않으면 지정된 색상으로 생성
;;;      - is-numeric : 문자열이 숫자로 변환 가능한지 확인 (True/False)
;;;      - parse-h-spec : "H 298x201..." 형식의 문자열을 파싱하여 리스트 (298 201...)로 변환
;;;      - format-depth-text : 깊이 값을 소수점 1자리 문자열로 포맷팅 (ex: "5.0")
;;;      - tsp-sanitize-project-name : 파일명 및 CSV에 사용할 수 없는 특수문자를 언더바(_)로 치환
;;;      - tsp-get-project-list : 저장된 프로젝트 목록 반환 (직접 순회 방식)
;;;      - tsp-generate-default-project-name : 중복되지 않는 다음 프로젝트 이름 생성 (Project_N+1)
;;;      - tsp-save-project-data : 프로젝트 데이터 저장
;;;      - tsp-shape-mismatch-prompt : 경계선 형상/선분 수 불일치 시 [수동 데이터 매칭] 선택 팝업
;;;
;;; ▶ [TSP_1B_Globals_Utils.lsp] (565줄 / 29.4KB) - 전역 변수 및 공통/그룹 유틸리티 (Part B)
;;;    역할: 해치 패턴(16종), 표준 H형강 규격, 전역 설정 변수, 레이어/해치 유틸리티, 프로젝트 저장/불러오기/자동매칭, 도면 그룹(ACAD_GROUP) 제어 [tsp-load-project-data, tsp-delete-project-data, tsp-pm-build-test-segs, tsp-pm-shape-match-p, tsp-pm-find-matches 외 7개]
;;;    포함 함수 (12개):
;;;      - tsp-load-project-data : 데이터 불러오기 (형상일치 검증, 불일치 시 수동 데이터 매칭 연동 및 좌표 보호)
;;;      - tsp-delete-project-data : 데이터 딕셔너리에서 프로젝트 삭제
;;;      - tsp-pm-build-test-segs : 정점 리스트로 비교용 세그먼트(LENGTH/ANGLE) 생성 (init-segment-data와 동일 계산식)
;;;      - tsp-pm-shape-match-p : 두 세그먼트 리스트의 형상 일치 여부 (기존 불러오기 검증 기준과 동일)
;;;      - tsp-pm-find-matches : 저장된 전체 프로젝트 중 형상이 일치하는 목록 반환 ((프로젝트명 . 매칭시작정점) ...)
;;;      - tsp-pm-tag-boundary : 경계선 객체에 프로젝트명(TSP_PROJ_NAME) 각인 (유효한 객체일 때만)
;;;      - tsp-pm-choose-candidate : 형상 일치 후보가 여러 개일 때 기존 프로젝트 목록 팝업(tsp_load_list)으로 선택
;;;      - tsp-pm-auto-select-project : 선택한 경계선에 맞는 프로젝트 탐색
;;;      - save-prompt-dialog-callback : 저장 버튼 클릭 시 팝업창 띄우기 (예:1, 아니오:2, 취소:0 반환)
;;;      - tsp-get-entity-handle : 엔티티 고유 핸들(Handle) ID를 추출
;;;      - tsp-get-intersection-point : 두 직선(무한)의 교차점을 계산
;;;      - tsp-clear-drawing-group : 특정 경계선에 종속된 기존 작도 그룹을 찾아 안전하게 삭제 및 분해하는 함수
;;;
;;; ▶ [TSP_1C_Globals_Utils.lsp] (173줄 / 7.5KB) - 전역 변수 및 공통/그룹 유틸리티 (Part C)
;;;    역할: 해치 패턴(16종), 표준 H형강 규격, 전역 설정 변수, 레이어/해치 유틸리티, 프로젝트 저장/불러오기/자동매칭, 도면 그룹(ACAD_GROUP) 제어 [tsp-group-last-entities, tsp-apply-custom-hatch, tsp-get-rock-start-depth]
;;;    포함 함수 (3개):
;;;      - tsp-group-last-entities : 생성된 객체고유 그룹화
;;;      - tsp-apply-custom-hatch : 커스텀 해치 패턴 PAT 생성 및 원점(Origin)/스케일 강제 지정
;;;      - tsp-get-rock-start-depth : 지층 데이터에서 숏크리트 적용 암반선의 심도를 탐색하여 반환 (단위: m)
;;;
;;; ▶ [TSP_2A_Geometry.lsp] (584줄 / 25.8KB) - 기하학 좌표 계산, 모서리 편집 및 세그먼트 초기화 (Part A)
;;;    역할: 폴리라인 정점 추출, CW/CCW 방향 및 오목/볼록 판별, 코너 H-Pile 회전각 계산, 대화형 모서리 추가/삭제 편집(D/A), 세그먼트 리스트 초기화 [extract-vertices, get-polygon-orientation, is-closed-polyline, is-corner-convex, get-dist-point-to-segment 외 12개]
;;;    포함 함수 (17개):
;;;      - extract-vertices : 엔티티(LWPOLYLINE 등)에서 정점 좌표(Vertex) 리스트 추출
;;;      - get-polygon-orientation : 폴리곤의 방향(CW/CCW) 판별 (1: CCW, -1: CW)
;;;      - is-closed-polyline : 폴리라인이 폐합되었는지 확인 (Flag 70 또는 시작점=끝점)
;;;      - is-corner-convex : 코너가 볼록한지 오목한지 판별 (외적 활용)
;;;      - get-dist-point-to-segment : 점과 선분 사이의 최단 거리 계산
;;;      - calc-orientation-by-closest-segment : 가상 연장선을 포함한 최적 방향 판별
;;;      - determine-boundary-orientation : 경계선의 내부/외부 방향을 사용자 입력(가상 연장선 적용) 또는 자동 판별
;;;      - calc-hpile-vertices : 중심점 기준으로 H-Pile 형상 점 좌표 계산 (12개 정점 + 4개 Fillet)
;;;      - tsp-get-closest-vertex-index : 정점 리스트 재배열
;;;      - tsp-shift-vertex-list : 
;;;      - tsp-filter-collinear-vertices : 180도 근사(꺾임각 tol-deg 이내) 절점을 자동 제외하여 반환
;;;      - tsp-save-custom-vertices-ldata : 편집된 모서리 정점들을 원본 0번 정점 기준 상대 오프셋으로 저장 (복사본 경계선 호환)
;;;      - tsp-load-custom-vertices-ldata : 저장된 상대 오프셋으로부터 현재 경계선 위치에 맞는 모서리 정점 리스트 복원
;;;      - tsp-draw-corner-edit-preview : 모서리 편집 중 현재 모서리 번호(_지시)와 세그먼트 번호(_번호)를 임시로 화면에 표시
;;;      - tsp-get-corner-idx-from-ent : 클릭한 _지시 객체로부터 모서리 인덱스를 추출 (XDATA 우선, 없을 시 최근접 정점 탐색)
;;;      - tsp-insert-vertex-on-ordered-list : 정렬된 모서리 리스트(verts)에서 클릭한 점(pt-on-pl)이 속한 세그먼트 구간 사이에 정점 삽입
;;;      - tsp-interactive-edit-corners : 경계선 선택 후 모서리(세그먼트) 삭제(D) / 추가(A) 대화형 편집 루프
;;;
;;; ▶ [TSP_2B_Geometry.lsp] (300줄 / 14.4KB) - 기하학 좌표 계산, 모서리 편집 및 세그먼트 초기화 (Part B)
;;;    역할: 폴리라인 정점 추출, CW/CCW 방향 및 오목/볼록 판별, 코너 H-Pile 회전각 계산, 대화형 모서리 추가/삭제 편집(D/A), 세그먼트 리스트 초기화 [init-segment-data]
;;;    포함 함수 (1개):
;;;      - init-segment-data : 상위에서 정렬된 vertices를 받아 세그먼트 초기화
;;;
;;; ▶ [TSP_3A_Section.lsp] (260줄 / 18.6KB) - 단면도 및 지반 주상도 작도 (Part A)
;;;    역할: 단면 띠장/유압잭/스크류잭 블록 정의, 지하수위 기호, 지반 주상도 및 N치 작도, H-Pile+토류판/CIP 수직 단면도, 단면 지보재/브래킷/숏크리트/그라우팅 작도 [create-wale-section-block, draw-water-level-symbol, draw-soil-column, draw-wale-section, draw-vertical-hpile]
;;;    포함 함수 (5개):
;;;      - create-wale-section-block : 단면 띠장(Wale) 블록 정의
;;;      - draw-water-level-symbol : 지하수위 기호(삼각형 역상) 및 수위 텍스트 작도
;;;      - draw-soil-column : 지반 주상도(Soil Column) 생성 및 지층별 해치/텍스트 표기
;;;      - draw-wale-section : 띠장 삽입 및 보걸이 상세 작도
;;;      - draw-vertical-hpile : H-Pile 수직 단면선 작도
;;;
;;; ▶ [TSP_3B_Section.lsp] (280줄 / 17.4KB) - 단면도 및 지반 주상도 작도 (Part B)
;;;    역할: 단면 띠장/유압잭/스크류잭 블록 정의, 지하수위 기호, 지반 주상도 및 N치 작도, H-Pile+토류판/CIP 수직 단면도, 단면 지보재/브래킷/숏크리트/그라우팅 작도 [draw-section-hpile]
;;;    포함 함수 (1개):
;;;      - draw-section-hpile : 단면 H-Pile, 토류판, 띠장, 지보재(버팀보/잭/앵커), 숏크리트 등 통합 작도
;;;
;;; ▶ [TSP_3C_Section.lsp] (501줄 / 27.2KB) - 단면도 및 지반 주상도 작도 (Part C)
;;;    역할: 단면 띠장/유압잭/스크류잭 블록 정의, 지하수위 기호, 지반 주상도 및 N치 작도, H-Pile+토류판/CIP 수직 단면도, 단면 지보재/브래킷/숏크리트/그라우팅 작도 [draw-section-cip, make-layer-if-not-exist, create-strut-jack-block, create-strut-screw-jack-block, tsp-load-subzone-to-globals]
;;;    포함 함수 (5개):
;;;      - draw-section-cip : CIP 단면도 생성
;;;      - make-layer-if-not-exist : 모듈 내 정의 함수
;;;      - create-strut-jack-block : 유압잭 형상을 블록으로 정의
;;;      - create-strut-screw-jack-block : 스크류잭 형상을 블록으로 정의
;;;      - tsp-load-subzone-to-globals : 선택한 세부 구간(SUB-ZONE)의 벽체/지층/수위/지보재 세트를 전역 변수에 적용
;;;
;;; ▶ [TSP_3D_Section.lsp] (326줄 / 16.2KB) - 단면도 및 지반 주상도 작도 (Part D)
;;;    역할: 단면 띠장/유압잭/스크류잭 블록 정의, 지하수위 기호, 지반 주상도 및 N치 작도, H-Pile+토류판/CIP 수직 단면도, 단면 지보재/브래킷/숏크리트/그라우팅 작도 [tsp-filter-supports-for-section, draw-section-ground-line]
;;;    포함 함수 (2개):
;;;      - tsp-filter-supports-for-section : 단면도 작도 전 구간(SUB-ZONE) 세트 전환 및 지보재 필터링
;;;      - draw-section-ground-line : 단면도 그리기 메인 진입점 (지반선 그리기 및 UCS 변환)
;;;
;;; ▶ [TSP_4A_Plan.lsp] (569줄 / 28.6KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part A)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [create-wale-offsets, make-wale-lw, get-corner-upg-vals, get-corner-splice-length, draw-wale-subsegment]
;;;    포함 함수 (5개):
;;;      - create-wale-offsets : 각 세그먼트 데이터에 맞춰 띠장(Wale)생성 (사전 검사 동기화 및 마감 완벽 적용)
;;;      - make-wale-lw : [보조 1] LWPOLYLINE 강제 생성
;;;      - get-corner-upg-vals : [보조 2] 코너의 상향(Upgrade) 띠장 데이터 추출
;;;      - get-corner-splice-length : 외부 H-Pile 탐색 및 수동 선택 범위 처리 로직 통합
;;;      - draw-wale-subsegment : [보조 4] 지정된 도막(Sub-segment)의 띠장 라인 4줄 작도
;;;
;;; ▶ [TSP_4B_Plan.lsp] (329줄 / 17.1KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part B)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [calculate-corner-params, get-hpile-limit-offset, create-hpile-section, create-hpile-section-block, create-post-pile-block 외 5개]
;;;    포함 함수 (10개):
;;;      - calculate-corner-params : 코너 부위에서 H-Pile의 회전각 및 삽입 위치 계산
;;;      - get-hpile-limit-offset : H-Pile 배치 시 플랜지 간섭을 피하기 위한 한계 오프셋 계산
;;;      - create-hpile-section : H-Pile 폴리라인 형상 생성
;;;      - create-hpile-section-block : H-Pile 단면 형상을 블록으로 정의
;;;      - create-post-pile-block : 중간말뚝(Post Pile) 블록 정의
;;;      - create-timber-panel-object : 평면도 토류판 폴리라인 및 해치 생성
;;;      - place-hpile-at-corner-simple : 코너 부위에 단일 H-Pile 블록 배치
;;;      - tsp-geo-station : [경계선 추종] 설계 현(chord) 기준 거리 v 를
;;;      - tsp-geo-augment-chain : [경계선 추종] 설계 체인점 사이에 경계선의 실제 정점을 삽입
;;;      - tsp-geo-arc-of : [경계선 추종] 현 기준 거리 v -> 경계선 호장 위치(절대값)
;;;
;;; ▶ [TSP_4C_Plan.lsp] (517줄 / 35.4KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part C)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [place-hpile-timber-along-boundary, make-lw-line]
;;;    포함 함수 (2개):
;;;      - place-hpile-timber-along-boundary : 평면도 H-Pile/토류판 통합 작도
;;;      - make-lw-line : 선분 생성 보조
;;;
;;; ▶ [TSP_4D_Plan.lsp] (154줄 / 8.4KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part D)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-draw-closed-poly, tsp-draw-wedge-connection, get-poly-segs]
;;;    포함 함수 (3개):
;;;      - tsp-draw-closed-poly : 폴리라인 작도 범용 유틸리티 (모듈화 분리)
;;;      - tsp-draw-wedge-connection : 화타쐐기 작도 및 가림 판정용 다각형 외곽선 반환
;;;      - get-poly-segs : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_4E_Plan.lsp] (742줄 / 48.8KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part E)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-draw-all-corner-braces, valid-hit]
;;;    포함 함수 (2개):
;;;      - tsp-draw-all-corner-braces : 사보강재(Corner Strut) 및 다중 대칭 보강재(Brace Array) 작도 통합 모듈
;;;      - valid-hit : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_4F_Plan.lsp] (385줄 / 20.7KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part F)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-draw-hpile-grouting-per-segment, tsp-boundary-path-info, tsp-dist-of-near-vertex, tsp-expand-segments-by-geometry]
;;;    포함 함수 (4개):
;;;      - tsp-draw-hpile-grouting-per-segment : H-Pile+토류판 세그먼트별 배면 차수그라우팅 평면 작도
;;;      - tsp-boundary-path-info : 경계선 폴리라인의 원본 정점 / 누적거리 / 총길이 / 폐합여부 반환
;;;      - tsp-dist-of-near-vertex : 정점 리스트에서 pt와 가장 가까운 원본 정점의 누적거리 반환
;;;      - tsp-expand-segments-by-geometry : [경계선 추종] 설계 세그먼트를 경계선 실제 형상을 따르는
;;;
;;; ▶ [TSP_4G_Plan.lsp] (588줄 / 30.2KB) - 평면도 통합 작도 제어 및 버팀보(Strut) 평면 자동 재작도 (Part G)
;;;    역할: 평면도 전체 생성 총괄(벽체 H-Pile/CIP 분기, 사보강재, 띠장 선행 작도 후 SUPPORT-LIST 기반 버팀보 평면 일괄 재작도 및 번호표 생성) [create-hpile-set-on-boundary, tsp-ceil, tsp-create-cip-guideline, tsp-calc-cip-layout, tsp-draw-cip-elements 외 3개]
;;;    포함 함수 (8개):
;;;      - create-hpile-set-on-boundary : 평면도 통합 생성 메인 함수 (H-Pile/C.I.P 벽체, 사보강재, 띠장 작도 및 SUPPORT-LIST 기반 평면 버팀보 자동 재작도 통합)
;;;      - tsp-ceil : 숫자 올림 보조 함수
;;;      - tsp-create-cip-guideline : 외측 방향을 고려한 D/2 오프셋 보조선 생성
;;;      - tsp-calc-cip-layout : 경계선(곡선)을 따라 C.I.P 중심점 배열 및 간격 계산
;;;      - tsp-draw-cip-elements : C.I.P 배열 요소를 작도하고, 브래킷을 생성하는 함수
;;;      - draw-bracket-poly : 모듈 내 정의 함수
;;;      - tsp-acos : 아크코사인 함수 (수학 연산 지원)
;;;      - tsp-normalize-angle : 각도를 0 ~ 2pi 사이로 정규화
;;;
;;; ▶ [TSP_4H_Plan.lsp] (144줄 / 6.6KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part H)
;;;    역할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-ang-dist, get-grouting-arc-angles, tsp-math-normalize-angle, tsp-draw-advanced-grouting]
;;;    포함 함수 (4개):
;;;      - tsp-ang-dist : 두 각도의 차이(최단거리) 산출
;;;      - get-grouting-arc-angles : 그라우팅 원과 CIP 원의 겹침을 잘라내기 위한 시작/끝 각도 산출
;;;      - tsp-math-normalize-angle : 각도를 0 ~ 2PI 사이로 정규화
;;;      - tsp-draw-advanced-grouting : 수학적 연산을 통한 완벽한 교차 호(Arc) 작도 엔진
;;;
;;; ▶ [TSP_4I_Plan.lsp] (455줄 / 24.1KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part I)
;;;    역할: 연속 CIP 경로 오프셋, 코너/이종접합부 한계 탐색, 겹침 분배 및 H-Pile 배치 시뮬레이션 [tsp-safe-vla-offset, tsp-update-gap, tsp-cip-calc-chain-layout]
;;;    포함 함수 (3개):
;;;      - tsp-safe-vla-offset : vla-offset 안전 래퍼 (실패 시 nil 반환 - 작도 중단 방지)
;;;      - tsp-update-gap : 리스트의 특정 인덱스(idx) 요소를 val로 교체하여 반환
;;;      - tsp-cip-calc-chain-layout : 연속 CIP 체인의 코너 선탐색, 직선 구간 겹침 분배 및 H-Pile 시뮬레이션 수행
;;;
;;; ▶ [TSP_4J_Plan.lsp] (665줄 / 34.0KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part J)
;;;    역할: 연속 CIP 본체/브래킷/앵커/배면 그라우팅 평면 작도 및 CIP 유틸리티 [tsp-draw-cip-elements-and-grout, tsp-draw-cip-plan-per-segment, tsp-find-next-dist-by-chord, tsp-make-segment-guideline, tsp-draw-cip-plan]
;;;    포함 함수 (6개):
;;;      - tsp-draw-cip-elements-and-grout : 산출된 CIP 배열을 기반으로 CIP/H-Pile/브래킷/앵커 및 배면 그라우팅 작도
;;;      - draw-bracket-poly : 모듈 내 정의 함수
;;;      - tsp-draw-cip-plan-per-segment : 세그먼트별 연속 CIP 작도 및 브래킷 통합
;;;      - tsp-find-next-dist-by-chord : 현재 점에서 목표 현의 길이(직선 거리)를 만족하는 곡선상의 다음 누적 거리를 이분법으로 탐색
;;;      - tsp-make-segment-guideline : 세그먼트 구간 보조선 생성 (직접 좌표 계산으로 Offset 오류 원천 차단)
;;;      - tsp-draw-cip-plan : C.I.P 평면도 작도 메인 제어 흐름
;;;
;;; ▶ [TSP_4K_Plan.lsp] (440줄 / 23.2KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part K)
;;;    역할: 세그먼트/모서리 번호표(캡슐) 작도 및 사보강재 한계거리 산출 [draw-segment-numbers, draw-corner-numbers, draw-capsule-shape, check-wedge-collision, check-boundary-collision, tag-last-corner-ent, tsp-calculate-brace-limit]
;;;    포함 함수 (7개):
;;;      - draw-segment-numbers : 정렬된 vertices를 인자로 받아 세그먼트 번호 표기
;;;      - draw-corner-numbers : 모서리 지시선 작도
;;;      - draw-capsule-shape : 모서리 번호 캡슐 작도
;;;      - check-wedge-collision : 모서리 번호표 간섭 검사 (한계치 3000으로 완화)
;;;      - check-boundary-collision : 지시선이 경계선과 교차하거나 완전히 겹치는지 검사
;;;      - tag-last-corner-ent : 모듈 내 정의 함수
;;;      - tsp-calculate-brace-limit : 사보강재 한계거리(절대/권장) 산출
;;;
;;; ▶ [TSP_4L_Plan.lsp] (218줄 / 9.4KB) - 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part L)
;;;    역할: 삼각형 내부 판별 및 세그먼트별 수동 중간말뚝 삭제/복원 [tsp-is-point-in-triangle, sign-func, tsp-delete-manual-post-by-segment, process-and-delete, process-deleted-pts]
;;;    포함 함수 (5개):
;;;      - tsp-is-point-in-triangle : 한 점이 세 꼭짓점으로 이루어진 삼각형 내부에 있는지 판별
;;;      - sign-func : 모듈 내 정의 함수
;;;      - tsp-delete-manual-post-by-segment : 중간말뚝 수동 추가/삭제/초기화
;;;      - process-and-delete : 모듈 내 정의 함수
;;;      - process-deleted-pts : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_5A_DCL_and_Strut.lsp] (45줄 / 2.9KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part A)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-get-exact-ry, tsp-calc-unsupported-length, tsp-project-dist]
;;;    포함 함수 (3개):
;;;      - tsp-get-exact-ry : H형강의 제원(H, B, tw, tf)을 적분하여 약축 단면 2차 반경(ry) 정밀 계산
;;;      - tsp-calc-unsupported-length : 세장비 한계와 단면 제원을 바탕으로 허용 비지지길이(m) 산출
;;;      - tsp-project-dist : 특정 각도(dir) 축에 대한 목표점의 투영 거리 산출 (스칼라 내적)
;;;
;;; ▶ [TSP_5B_DCL_and_Strut.lsp] (792줄 / 61.6KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part B)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [create-tsp-dcl]
;;;    포함 함수 (1개):
;;;      - create-tsp-dcl : 임시 DCL 파일을 생성하여 각종 UI 다이얼로그의 형태를 정의하고, 해당 파일의 경로를 반환하는 함수
;;;
;;; ▶ [TSP_5C_DCL_and_Strut.lsp] (470줄 / 25.8KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part C)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-create-cip-dcl, tsp-cip-update-hpile-dims, tsp-cip-update-wale-dims, tsp-cip-update-mode, tsp-run-cip-dialog 외 4개]
;;;    포함 함수 (9개):
;;;      - tsp-create-cip-dcl : C.I.P 전용 DCL 다이얼로그 파일 생성
;;;      - tsp-cip-update-hpile-dims : H-Pile 규격에 맞춰 치수 박스 및 직경(D) 자동 업데이트
;;;      - tsp-cip-update-wale-dims : 띠장 규격에 맞춰 치수 박스 자동 업데이트
;;;      - tsp-cip-update-mode : 배열 모드 변경 시 겹침량 입력창 활성/비활성 제어
;;;      - tsp-run-cip-dialog : C.I.P 설정 대화상자 실행 및 콜백 제어
;;;      - section-choice-dialog-callback : 단면도 생성 시 세그먼트/모서리 선택 다이얼로그 콜백
;;;      - tsp-apply-settings-to-all : 선택한 세그먼트의 설정값을 모든 세그먼트에 일괄 적용
;;;      - tsp-apply-soil-to-selected : 선택된 세그먼트들에 지반 정보 및 숏크리트 설정 복사
;;;      - tsp-apply-support-to-selected : 선택된 세그먼트들에 지보재 정보 복사
;;;
;;; ▶ [TSP_5D_DCL_and_Strut.lsp] (511줄 / 24.8KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part D)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-apply-wall-to-selected, tsp-is-brace-eligible-corner-p, tsp-apply-brace-to-selected, tsp-reset-segment-data, tsp-reset-all-segments 외 5개]
;;;    포함 함수 (10개):
;;;      - tsp-apply-wall-to-selected : 선택된 세그먼트들에 흙막이벽 정보 복사
;;;      - tsp-is-brace-eligible-corner-p : 해당 세그먼트 시작 코너(idx)가 기하학적으로 사보강재 설치 가능한 볼록 코너인지 판별
;;;      - tsp-apply-brace-to-selected : 선택된 세그먼트들에 사보강재 정보 복사 (기하학적 유효 코너 자동 복구 및 결과 반환)
;;;      - tsp-reset-segment-data : 특정 세그먼트의 설정을 초기화
;;;      - tsp-reset-all-segments : 모든 세그먼트의 설정을 일괄 초기화
;;;      - scale-dialog-callback : 스케일 설정 대화상자 콜백
;;;      - project-list-callback : 프로젝트 불러오기 및 삭제 목록 팝업
;;;      - main-dialog-callback : 메인 선택 대화상자 콜백
;;;      - update-list-item : 리스트의 특정 인덱스(idx) 요소를 new-val로 안전하게 교체
;;;      - input-segment-wizard : [입력 모드] 단계별 마법사 방식
;;;
;;; ▶ [TSP_5E_DCL_and_Strut.lsp] (569줄 / 26.4KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part E)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [modify-segment-dashboard, format-h-spec-short, get-segment-display-string, reset-globals-to-defaults, update-segment-manager-list 외 6개]
;;;    포함 함수 (11개):
;;;      - modify-segment-dashboard : [수정 모드] 통합 메뉴 대시보드 방식
;;;      - format-h-spec-short : H형강 규격을 폭x높이 형태로 짧게 변환하고 2중 표기 적용 (빈 값 nil 보호 로직 추가)
;;;      - get-segment-display-string : 세그먼트 데이터를 리스트박스 표시용 문자열로 변환
;;;      - reset-globals-to-defaults : 입력 버튼 클릭 시 전역 변수를 시스템 기본값으로 초기화
;;;      - update-segment-manager-list : 관리자 리스트박스 갱신
;;;      - sync-globals-to-segment : 전역 변수(DCL입력값)를 세그먼트 데이터 리스트로 저장
;;;      - load-segment-to-globals : 세그먼트 데이터를 전역 변수로 복원
;;;      - tsp-parse-space-string : 띄어쓰기로 구분된 문자열을 정수 리스트로 변환 (다중 선택 처리용)
;;;      - tsp-join-space-string : 정수 리스트를 띄어쓰기로 구분된 문자열로 변환 (다중 선택 UI 복원용)
;;;      - tsp-pause-for-measurement : DCL 창을 임시로 숨기고 도면을 제어/측정할 수 있게 해주는 기능
;;;      - *error* : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_5F_DCL_and_Strut.lsp] (450줄 / 24.9KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part F)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [manager-dialog-callback]
;;;    포함 함수 (1개):
;;;      - manager-dialog-callback : [메인] 다중 선택(Multi-Select) 설정 및 부분 복사 로직 적용
;;;
;;; ▶ [TSP_5G_DCL_and_Strut.lsp] (184줄 / 10.2KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part G)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [draw-tsp-legend, tsp-get-abbr, tsp-format-soil-name]
;;;    포함 함수 (3개):
;;;      - draw-tsp-legend : 지반 범례(Legend) 도면 작도
;;;      - tsp-get-abbr : 모듈 내 정의 함수
;;;      - tsp-format-soil-name : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_5H_DCL_and_Strut.lsp] (303줄 / 22.0KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part H)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [soil-dialog-callback]
;;;    포함 함수 (1개):
;;;      - soil-dialog-callback : 지반 정보 입력 메인 대화상자 콜백
;;;
;;; ▶ [TSP_5I_DCL_and_Strut.lsp] (133줄 / 9.0KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part I)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [hpile-dialog-callback]
;;;    포함 함수 (1개):
;;;      - hpile-dialog-callback : H-Pile 규격 설정 대화상자 콜백
;;;
;;; ▶ [TSP_5J_DCL_and_Strut.lsp] (799줄 / 49.4KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part J)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [brace-dialog-callback, tsp-clean-manual-posts-for-deleted-brace]
;;;    포함 함수 (2개):
;;;      - brace-dialog-callback : 다단 사보강재(Corner Strut) 설정 대화상자 콜백
;;;      - tsp-clean-manual-posts-for-deleted-brace : 허용 길이 초과 여부를 검사하여 필요시 2H로 자동 변환
;;;
;;; ▶ [TSP_5K_DCL_and_Strut.lsp] (100줄 / 5.4KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part K)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-get-wale-offset, tsp-strut-preview-restore]
;;;    포함 함수 (2개):
;;;      - tsp-get-wale-offset : 특정 세그먼트의 띠장 내측면까지의 오프셋 거리 계산 (지보재 설정 띠장 최우선)
;;;      - tsp-strut-preview-restore : [직접 작도 취소 시] 작도 전 원래 좌표(orig-pts)로 버팀보 미리보기 재표시
;;;
;;; ▶ [TSP_5L_DCL_and_Strut.lsp] (3069줄 / 170.7KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part L)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [support-dialog-callback, tsp-safe-hide-ent, strut-input-dialog, anchor-input-dialog, tsp-anchor-edit-plan-angle 외 16개]
;;;    포함 함수 (21개):
;;;      - support-dialog-callback : 모듈 내 정의 함수
;;;      - tsp-safe-hide-ent : 물리적 삭제(entdel) 대신 객체를 안전하게 숨기는 함수
;;;      - strut-input-dialog : 버팀보 입력창
;;;      - anchor-input-dialog : 앵커 입력창 (단별 띠장 규격 및 적용 구간 보존 포함)
;;;      - tsp-anchor-edit-plan-angle : 개별 앵커 평면각 조절 루프
;;;      - tsp-get-nearest-vertex : 클릭한 점과 가장 가까운 정점(Base Point) 반환
;;;      - tsp-get-connected-anchor-entities : 앵커 전체 묶음(선/해치) 수집
;;;      - tsp-anc-ent-pts : 앵커 객체 정점 목록 (LWPOLYLINE 10 / LINE 10,11)
;;;      - tsp-anc-bracket-mid : 정착구(닫힌 4점 폴리선)이면 1-2번 정점 중점(=앵커 기준점) 반환
;;;      - tsp-anc-pt-near-list : 점이 점 목록 중 하나와 허용오차 내 일치하는지
;;;      - tsp-anc-pt-seg-dist : 점과 선분 사이 최단거리 (2D)
;;;      - tsp-get-anchor-group : 클릭한 앵커 1개의 (기준점 객체목록) 반환
;;;      - tsp-anc-nearest-seg : 점에서 가장 가까운 세그먼트의 (V-START V-END) 반환
;;;      - tsp-anc-measure-offset : 도면상 앵커가 세그먼트 법선에서 회전된 각도(도, 반시계 +) 측정
;;;      - tsp-has-strut-at-depth-p : 지보재 목록에 동일 설치깊이의 버팀보가 존재하는지 확인
;;;      - tsp-get-prev-stage-depth : 대상 깊이보다 얕은(작은) 직전 지보재 단계의 최대 깊이 반환
;;;      - tsp-normalize-strut-subgroups : 사용자 입력 이름을 보존하며 설치깊이 오름차순(동일 깊이 시 버팀보가 위, 앵커가 아래) 정렬
;;;      - update-support-list : 지보재 리스트를 DCL에 출력 (단별 띠장 규격 및 적용 구간 표시 포함)
;;;      - tsp-handle-btn-add : 모듈 내 정의 함수
;;;      - tsp-handle-btn-modify : 모듈 내 정의 함수
;;;      - tsp-pick-seg-hpile-pt : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_5M_DCL_and_Strut.lsp] (673줄 / 29.0KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part M)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-shift-anchor-angles, tsp-get-support-item-wale-spec, tsp-get-support-item-wale-vals, tsp-get-support-item-range-pts, tsp-set-support-item-range-pts 외 29개]
;;;    포함 함수 (34개):
;;;      - tsp-shift-anchor-angles : 지보재 목록 내 앵커 평면각 기록(9번 인덱스) 좌표 이동 보정
;;;      - tsp-get-support-item-wale-spec : 개별 지보재 항목의 띠장 규격 문자열 반환 (미지정 시 기본 띠장 반환)
;;;      - tsp-get-support-item-wale-vals : 개별 지보재 항목의 띠장 제원 리스트 (H B tw tf) 반환
;;;      - tsp-get-support-item-range-pts : 개별 지보재 항목에 지정된 적용 구간 좌표 (pt1 pt2) 반환 (전체면 nil)
;;;      - tsp-set-support-item-range-pts : 개별 지보재 항목에 적용 구간 좌표 (pt1 pt2) 또는 nil 저장
;;;      - tsp-get-support-item-dist-interval : 세그먼트 시작점(v-start)과 각도(v-angle) 기준으로 적용 구간 (d1 . d2) 반환
;;;      - tsp-shift-support-ranges : 경계선 이동 시 지보재 적용 구간 좌표(range-pts)도 함께 이동 보정
;;;      - tsp-shift-anchor-angles-in-segs : 세그먼트 목록 전체의 앵커 평면각 및 적용 구간 기록 좌표 이동 보정
;;;      - tsp-build-poly-verts : 내부판정(PIP)용 경계선 정점 리스트 생성
;;;      - tsp-is-point-inside : 점이 다각형(폴리라인) 내부에 있는지 판별 (Ray-casting 알고리즘)
;;;      - tsp-brace-opt-get : 모듈 내 정의 함수
;;;      - tsp-brace-opt-set : 모듈 내 정의 함수
;;;      - tsp-corbel-deleted-p : 모듈 내 정의 함수
;;;      - tsp-corbel-adopt-ents : 모듈 내 정의 함수
;;;      - tsp-shift-deleted-corbels : 모듈 내 정의 함수
;;;      - tsp-manual-delete-corbel : 모듈 내 정의 함수
;;;      - tsp-reset-deleted-corbels : 모듈 내 정의 함수
;;;      - tsp-corbel-default-str : 모듈 내 정의 함수
;;;      - tsp-ctc-mm : 모듈 내 정의 함수
;;;      - tsp-1h-post-quadrant : 모듈 내 정의 함수
;;;      - tsp-1h-post-place : 모듈 내 정의 함수
;;;      - tsp-1h-post-reposition : 모듈 내 정의 함수
;;;      - tsp-2h-post-reposition : 이미 작도된 2H 자동 중간말뚝(TSP_POST_2H XDATA 보유)을 보강재 왼쪽(L)/오른쪽(R)으로 즉시 재배치
;;;      - tsp-1h-jack-want-top : 모듈 내 정의 함수
;;;      - tsp-1h-jack-at-pt2 : 모듈 내 정의 함수
;;;      - tsp-norm-angle-rad : 모듈 내 정의 함수
;;;      - tsp-angle-bisector : 모듈 내 정의 함수
;;;      - tsp-calc-strut-step-dist : 모듈 내 정의 함수
;;;      - tsp-get-seg-inward-normal : 모듈 내 정의 함수
;;;      - tsp-poly-from-segs : 모듈 내 정의 함수
;;;      - tsp-poly-center : 모듈 내 정의 함수
;;;      - tsp-pt-line-dist : 모듈 내 정의 함수
;;;      - tsp-wale-lines-geo : 모듈 내 정의 함수
;;;      - tsp-wale-lines-dwg : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_5N_DCL_and_Strut.lsp] - 국부 띠장 정합, 직교 C.T.C(방식 A) 버팀보 좌표 산출 및 각도 제어 (Part N)
;;;    역할: 다중 절점(꺾임) 세그먼트의 실제 국부 띠장 선분 탐색, 버팀보 부재 간 직각 거리(C.T.C) 기준 다음 버팀보 위치 산출, CSV 반올림 좌표 정밀 복원 및 양단 띠장 교차점 계산
;;;    포함 함수 (9개):
;;;      - tsp-pt-seg-dist-2d : 점과 유한 선분 사이의 2D 최단 수직거리 계산
;;;      - tsp-closest-pt-on-seg-2d : 점에서 유한 선분에 내린 수선의 발 반환
;;;      - tsp-get-local-wale-seg-info : 특정 지점이 속한 실제 경계선 도막 및 국부 띠장 내측선 정보 반환
;;;      - tsp-project-pick-to-seg-boundary : 기준점을 국부 띠장 내측선 수선의 발을 거쳐 경계선으로 역투영
;;;      - tsp-find-next-strut-base-by-perp-ctc : [방식 A] 이전 버팀보 축과 직각 거리(C.T.C)만큼 떨어진 다음 버팀보의 경계선 기준점 산출
;;;      - tsp-refine-strut-manual-pts : 저장된 버팀보 점 리스트(CSV 반올림 좌표 포함)를 직각 C.T.C 기준으로 정밀 복원
;;;      - tsp-solve-strut-ends : 버팀보 축과 양단 실제 띠장 내측선의 정밀 교차점 산출
;;;      - tsp-wale-fix-ends : 경계선 추종 띠장(1안) 및 도면 띠장(2안) 기준 양단 접합점 보정
;;;      - tsp-get-wale-info-near-pt : 버팀보 끝점 부근의 실제 띠장 내측선 정보 반환
;;;      - tsp-pick-strut-angle-interactive : 시작점 고정 후 끝점 클릭으로 버팀보 그룹 방향각 대화형 설정
;;;
;;; ▶ [TSP_5O_DCL_and_Strut.lsp] (1023줄 / 52.2KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part O)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-draw-manual-strut]
;;;    포함 함수 (1개):
;;;      - tsp-draw-manual-strut : 수동 버팀보 평면 객체 작도 (N등분/2H/말뚝/비평행띠장/경사버팀보 통합)
;;;
;;; ▶ [TSP_5P_DCL_and_Strut.lsp] (288줄 / 16.7KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part P)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-draw-realtime-continuous-braces]
;;;    포함 함수 (1개):
;;;      - tsp-draw-realtime-continuous-braces : 순수 좌표 추적 기반 연속 보강재 실시간 분할 작도 함수
;;;
;;; ▶ [TSP_5Q_DCL_and_Strut.lsp] (484줄 / 24.9KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part Q)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-manual-add-post, tsp-sanitize-support-list]
;;;    포함 함수 (2개):
;;;      - tsp-manual-add-post : 중간말뚝 수동추가
;;;      - tsp-sanitize-support-list : 지보재 리스트 내 오염 데이터 제거 및 ARX 크래시 방지용 강제 복원
;;;
;;; ▶ [TSP_5R_DCL_and_Strut.lsp] (210줄 / 9.6KB) - DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part R)
;;;    역할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-manual-delete-post, tsp-reset-post-manual]
;;;    포함 함수 (2개):
;;;      - tsp-manual-delete-post : 다중 영역/드래그 선택 및 연속 삭제
;;;      - tsp-reset-post-manual : 중간말뚝 수동 삭제 및 추가 기록 모두 초기화 및 도면 객체 청소
;;;
;;; ▶ [TSP_6A_Main_and_CSV.lsp] (514줄 / 25.3KB) - 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part A)
;;;    역할: C:TSP 메인 상태 머신, 실시간 평면 갱신(tsp-redraw-realtime-plan), 도면 N치 드래그 추출, CSV 내보내기/불러오기/매칭, 파이썬(GeoX) 다이렉트 연동, SUB-ZONES 전개 [tsp-zoom-to-right-half, tsp-zoom-legend, tsp-redraw-realtime-plan, tsp-fix-mirrored-polyline, tsp-split-string-by-comma 외 2개]
;;;    포함 함수 (7개):
;;;      - tsp-zoom-to-right-half : 평면도 전용 화면 우측 정렬 줌
;;;      - tsp-zoom-legend : 범례 전용 화면 우측 정렬 줌
;;;      - tsp-redraw-realtime-plan : 실시간 작도 시 도면(DWG)에 영구 저장된 중간말뚝 삭제 기록 필터링 유지
;;;      - tsp-fix-mirrored-polyline : 경계선 Z축 양수화(OCS 좌표계 재조정)
;;;      - tsp-split-string-by-comma : 문자열을 쉼표 단위로 분할하여 리스트 반환
;;;      - tsp-select-project-for-export : 내보낼 프로젝트를 DCL에서 선택
;;;      - tsp-export-name-prompt-callback : CSV 내보내기 시 프로젝트 이름 확인/수정 DCL 팝업창 호출
;;;
;;; ▶ [TSP_6B_Main_and_CSV.lsp] (617줄 / 28.7KB) - 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part B)
;;;    역할: C:TSP 메인 상태 머신, 실시간 평면 갱신(tsp-redraw-realtime-plan), 도면 N치 드래그 추출, CSV 내보내기/불러오기/매칭, 파이썬(GeoX) 다이렉트 연동, SUB-ZONES 전개 [tsp-write-project-csv, tsp-save-and-export-current-csv, tsp-export-to-csv, tsp-rebuild-boundary, tsp-merge-csv-seg-into-curr 외 6개]
;;;    포함 함수 (11개):
;;;      - tsp-write-project-csv : 지정된 프로젝트명(proj-name)의 데이터를 CSV 파일로 저장
;;;      - tsp-save-and-export-current-csv : 세그먼트 설정창에서 현재 프로젝트 저장(이름 변경 시 기존 프로젝트 덮어쓰기) 후 CSV 내보내기
;;;      - tsp-export-to-csv : 메인 메뉴에서 선택된 프로젝트를 CSV 파일로 출력
;;;      - tsp-rebuild-boundary : V-START 및 V-END 데이터를 분석하여 폴리라인 자동 생성
;;;      - tsp-merge-csv-seg-into-curr : 현재 세그먼트의 기하좌표(V-START/V-END/ANGLE/LENGTH)와 코너여부를 보존하며 CSV 단면 제원만 병합
;;;      - tsp-init-dummy-segments : 평면 경계선 없이 단면도만 작도할 때 사용할 기본 가상 세그먼트 리스트 생성
;;;      - tsp-parse-csv-file-data : CSV 파일을 파싱하여 new-seg-list를 반환하고 전역 설정을 복원하는 공통 파서
;;;      - tsp-csv-mapping-dialog : CSV 매칭 다이얼로그 제어
;;;      - update-map-list : 모듈 내 정의 함수
;;;      - tsp-apply-csv-to-current-segments : 세그먼트 설정창 내부에서 외부 CSV를 불러와 현재 세그먼트에 즉시 매핑 (방식 2)
;;;      - tsp-has-real-boundary-coords-p : 세그먼트 리스트에 실제 유효한 도면 경계선 좌표가 포함되어 있는지 판별
;;;
;;; ▶ [TSP_6C_Main_and_CSV.lsp] (548줄 / 29.4KB) - 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part C)
;;;    역할: C:TSP 메인 상태 머신, 실시간 평면 갱신(tsp-redraw-realtime-plan), 도면 N치 드래그 추출, CSV 내보내기/불러오기/매칭, 파이썬(GeoX) 다이렉트 연동, SUB-ZONES 전개 [tsp-import-from-csv, tsp-select-project-for-load, tsp-import-from-drawing]
;;;    포함 함수 (3개):
;;;      - tsp-import-from-csv : CSV 데이터를 읽어 도면 및 딕셔너리에 복원
;;;      - tsp-select-project-for-load : 불러올 프로젝트를 DCL에서 선택 (도면 내부용)
;;;      - tsp-import-from-drawing : 도면 내부 딕셔너리에서 데이터를 읽어 복원
;;;
;;; ▶ [TSP_6D_Main_and_CSV.lsp] (667줄 / 37.5KB) - 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part D)
;;;    역할: C:TSP 메인 상태 머신, 실시간 평면 갱신(tsp-redraw-realtime-plan), 도면 N치 드래그 추출, CSV 내보내기/불러오기/매칭, 파이썬(GeoX) 다이렉트 연동, SUB-ZONES 전개 [C:TSP, *error*]
;;;    포함 함수 (2개):
;;;      - C:TSP : 모듈 내 정의 함수
;;;      - *error* : 모듈 내 정의 함수
;;;
;;; ▶ [TSP_6E_Main_and_CSV.lsp] (561줄 / 27.0KB) - 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part E)
;;;    역할: C:TSP 메인 상태 머신, 실시간 평면 갱신(tsp-redraw-realtime-plan), 도면 N치 드래그 추출, CSV 내보내기/불러오기/매칭, 파이썬(GeoX) 다이렉트 연동, SUB-ZONES 전개 [tsp-extract-number-only, tsp-get-nvalue-from-drawing, tsp-open-manager-directly, tsp-direct-plan-from-python, tsp-direct-section-from-python 외 4개]
;;;    포함 함수 (9개):
;;;      - tsp-extract-number-only : 텍스트에서 숫자와 소수점('.')만 추출
;;;      - tsp-get-nvalue-from-drawing : 도면에서 텍스트를 드래그하여 N치 추출
;;;      - tsp-open-manager-directly : 다이렉트 작도 완료 후 사용자가 원할 때 세그먼트 설정창(Manager)으로 즉시 진입
;;;      - tsp-direct-plan-from-python : 파이썬 GUI에서 [평면도 즉시 작도] 클릭 시 실행
;;;      - tsp-direct-section-from-python : 파이썬 GUI에서 [단면도 즉시 작도] 클릭 시 실행
;;;      - tsp-get-zone-wall-stiffness : 구간(또는 세그먼트) 벽체 강성 점수 산출
;;;      - tsp-resolve-wall-spans : 중첩 구간 발생 시 강성 우선 원칙으로 비중첩 벽체 스팬 리스트 생성
;;;      - tsp-expand-segments-for-wall-plan : SUB-ZONES가 있는 세그먼트를 벽체 스팬별 가상 세그먼트로 전개
;;;      - tsp-log : 모듈 내 정의 함수
;;; ==========================================================================

(vl-load-com)

;;;;==========================================================================
;;;; [모듈 로더] TSP.lsp가 위치한 폴더를 자동 추적하여 하위 모듈들을 순서대로 로드합니다.
;;;;==========================================================================
(setq *tsp-module-files*
  '(
    "TSP_1A_Globals_Utils.lsp" ; 전역 변수 및 공통/그룹 유틸리티 (Part A) (546줄, 25.0KB)
    "TSP_1B_Globals_Utils.lsp" ; 전역 변수 및 공통/그룹 유틸리티 (Part B) (565줄, 29.4KB)
    "TSP_1C_Globals_Utils.lsp" ; 전역 변수 및 공통/그룹 유틸리티 (Part C) (173줄, 7.5KB)
    "TSP_2A_Geometry.lsp" ; 기하학 좌표 계산, 모서리 편집 및 세그먼트 초기화 (Part A) (584줄, 25.8KB)
    "TSP_2B_Geometry.lsp" ; 기하학 좌표 계산, 모서리 편집 및 세그먼트 초기화 (Part B) (300줄, 14.4KB)
    "TSP_3A_Section.lsp" ; 단면도 및 지반 주상도 작도 (Part A) (260줄, 18.6KB)
    "TSP_3B_Section.lsp" ; 단면도 및 지반 주상도 작도 (Part B) (280줄, 17.4KB)
    "TSP_3C_Section.lsp" ; 단면도 및 지반 주상도 작도 (Part C) (501줄, 27.2KB)
    "TSP_3D_Section.lsp" ; 단면도 및 지반 주상도 작도 (Part D) (326줄, 16.2KB)
    "TSP_4A_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part A) (569줄, 28.6KB)
    "TSP_4B_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part B) (329줄, 17.1KB)
    "TSP_4C_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part C) (517줄, 35.4KB)
    "TSP_4D_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part D) (154줄, 8.4KB)
    "TSP_4E_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part E) (742줄, 48.8KB)
    "TSP_4F_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part F) (385줄, 20.7KB)
    "TSP_4G_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part G) (588줄, 30.2KB)
    "TSP_4H_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part H) (144줄, 6.6KB)
        "TSP_4I_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part I) (455줄, 24.1KB)
    "TSP_4J_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part J) (665줄, 34.0KB)
    "TSP_4K_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part K) (440줄, 23.2KB)
    "TSP_4L_Plan.lsp" ; 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part L) (218줄, 9.4KB)
    "TSP_5A_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part A) (45줄, 2.9KB)
    "TSP_5B_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part B) (792줄, 61.6KB)
    "TSP_5C_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part C) (470줄, 25.8KB)
    "TSP_5D_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part D) (511줄, 24.8KB)
    "TSP_5E_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part E) (569줄, 26.4KB)
    "TSP_5F_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part F) (450줄, 24.9KB)
    "TSP_5G_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part G) (184줄, 10.2KB)
    "TSP_5H_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part H) (303줄, 22.0KB)
    "TSP_5I_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part I) (133줄, 9.0KB)
    "TSP_5J_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part J) (799줄, 49.4KB)
    "TSP_5K_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part K) (100줄, 5.4KB)
    "TSP_5L_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part L) (3069줄, 170.7KB)
    "TSP_5M_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part M) (673줄, 29.0KB)
    "TSP_5N_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part N) (207줄, 10.6KB)
    "TSP_5O_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part O) (1023줄, 52.2KB)
    "TSP_5P_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part P) (288줄, 16.7KB)
    "TSP_5Q_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part Q) (484줄, 24.9KB)
    "TSP_5R_DCL_and_Strut.lsp" ; DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part R) (210줄, 9.6KB)
    "TSP_6A_Main_and_CSV.lsp" ; 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part A) (514줄, 25.3KB)
    "TSP_6B_Main_and_CSV.lsp" ; 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part B) (617줄, 28.7KB)
    "TSP_6C_Main_and_CSV.lsp" ; 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part C) (548줄, 29.4KB)
    "TSP_6D_Main_and_CSV.lsp" ; 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part D) (667줄, 37.5KB)
    "TSP_6E_Main_and_CSV.lsp" ; 메인 명령어(C:TSP), 실시간 리드로우, CSV 입출력 및 파이썬 연동 (Part E) (561줄, 27.0KB)
  )
)

;;;;==========================================================================
;;;; defun tsp-get-base-dir : 현재 TSP.lsp 또는 하위 모듈이 위치한 폴더 경로 반환
;;;;==========================================================================
(defun tsp-get-base-dir ( / p)
  (cond
    ((setq p (findfile "TSP.lsp"))
     (vl-filename-directory p)
    )
    ((setq p (findfile "TSP_1A_Globals_Utils.lsp"))
     (vl-filename-directory p)
    )
    ((and (boundp '*tsp-base-dir*) *tsp-base-dir*)
     *tsp-base-dir*
    )
    (t nil)
  )
)

;;;;==========================================================================
;;;; defun tsp-load-all-modules : 분할된 모든 하위 모듈을 누락 없이 로드 (리로드 명령 지원)
;;;;==========================================================================
(defun tsp-load-all-modules ( / base-dir full-path ok-count fail-list mod-file load-err)
  (setq base-dir (tsp-get-base-dir))
  (if base-dir (setq *tsp-base-dir* base-dir))
  (setq ok-count 0)
  (setq fail-list '())

  (princ "\n========================================")
  (princ "\n[TSP] 모듈형 시스템을 로드합니다...")
  (foreach mod-file *tsp-module-files*
    (setq full-path
      (if base-dir
        (strcat base-dir "\\" mod-file)
        (findfile mod-file)
      )
    )
    (if (and full-path (findfile full-path))
      (progn
        (setq load-err (vl-catch-all-apply 'load (list full-path)))
        (if (vl-catch-all-error-p load-err)
          (progn
            (setq fail-list (append fail-list (list (strcat mod-file " (문법/로드 에러: " (vl-catch-all-error-message load-err) ")"))))
            (princ (strcat "\n  [실패] " mod-file " -> " (vl-catch-all-error-message load-err)))
          )
          (progn
            (setq ok-count (1+ ok-count))
            (princ (strcat "\n  [성공] " mod-file))
          )
        )
      )
      (progn
        (setq fail-list (append fail-list (list (strcat mod-file " (파일을 찾을 수 없음)"))))
        (princ (strcat "\n  [누락] " mod-file " 파일을 찾을 수 없습니다."))
      )
    )
  )

  (if fail-list
    (alert
      (strcat
        "[TSP 모듈 로드 경고]\n일부 모듈 파일을 불러오지 못했습니다.\n"
        "TSP.lsp와 하위 모듈 파일들이 같은 폴더에 있는지 확인하세요.\n\n"
        (apply 'strcat (mapcar '(lambda (s) (strcat "- " s "\n")) fail-list))
      )
    )
    (princ (strcat "\n[TSP] 전체 " (itoa ok-count) "개 모듈 로드 완료! (명령어: TSP / 모듈 재로드: TSP-RELOAD)"))
  )
  (princ "\n========================================\n")
  (princ)
)

;;;;==========================================================================
;;;; Command: TSP-RELOAD : 캐드 종료 없이 수정된 하위 모듈 파일들을 즉시 다시 불러오는 명령어
;;;;==========================================================================
(defun C:TSP-RELOAD ()
  (tsp-load-all-modules)
)

;; 메인 파일 로드 시 자동으로 전체 모듈 로드 실행
(tsp-load-all-modules)
(princ)
