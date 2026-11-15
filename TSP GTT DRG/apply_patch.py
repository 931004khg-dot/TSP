# -*- coding: utf-8 -*-
"""
apply_patch.py
- 사보강재(Corner Strut) 잭 설치 위치(한쪽 단부) 사용자 지정 기능 추가 패치
- 대상 파일:
  1. TSP_5B_DCL_and_Strut.lsp : 사보강재 세부 설정창(tsp_brace, tsp_brace_single)에 '잭 위치'(brace_jack_side) 팝업 추가
  2. TSP_5J_DCL_and_Strut.lsp : 인접 세그먼트 번호 기반 직관적 잭 위치 선택/수정/클릭동기화/반전 로직 구현
  3. TSP_4E_Plan.lsp          : 평면 사보강재 작도 시 단일(1H) 및 맞버팀대(2H) 모두 사용자 지정 잭 위치(AUTO/A/B/REV) 반영
"""
import os
import re
import shutil
import traceback


def find_target_file(filename):
    """스크립트 위치 기준 현재 폴더, 상위 폴더(..), 하위 폴더를 탐색하여 대상 파일 경로 반환 (대소문자 무관)"""
    base_dir = os.path.dirname(os.path.abspath(__file__))
    search_roots = [base_dir, os.path.abspath(os.path.join(base_dir, ".."))]
    target_lower = filename.lower()

    for root_dir in search_roots:
        if os.path.exists(root_dir):
            for entry in os.listdir(root_dir):
                full_path = os.path.join(root_dir, entry)
                if os.path.isfile(full_path) and entry.lower() == target_lower:
                    return full_path

    for root_dir in search_roots:
        for root, dirs, files in os.walk(root_dir):
            for f in files:
                if f.lower() == target_lower:
                    return os.path.join(root, f)

    return None


def build_normalized_Confirm_regex(old_block):
    """줄바꿈 및 연속된 공백/탭을 유연하게 매칭하는 정규표현식 생성"""
    tokens = re.split(r"\s+", old_block.strip())
    escaped_tokens = [re.escape(tok) for tok in tokens if tok]
    pattern_str = r"\s+".join(escaped_tokens)
    return re.compile(pattern_str, re.DOTALL)


def sanitize_for_cp949(text):
    """CP949(ANSI) 인코딩 시 에러를 유발할 수 있는 유니코드 특수 공백 문자를 일반 공백으로 정화"""
    special_spaces = [
        "\u00a0",
        "\u2000",
        "\u2001",
        "\u2002",
        "\u2003",
        "\u2004",
        "\u2005",
        "\u2006",
        "\u2007",
        "\u2008",
        "\u2009",
        "\u200a",
        "\u202f",
        "\u205f",
        "\u3000",
        "\ufeff",
    ]
    for sp in special_spaces:
        text = text.replace(sp, " ")
    return text


def read_lisp_file(path):
    """CP949 우선, UTF-8 폴백으로 리습 파일 읽기"""
    raw_bytes = open(path, "rb").read()
    try:
        return raw_bytes.decode("cp949")
    except UnicodeDecodeError:
        return raw_bytes.decode("utf-8")


def write_lisp_file(path, content, line_ending="\r\n"):
    """줄바꿈 정규화 및 CP949 정화 후 저장"""
    formatted = content.replace("\r\n", "\n").replace("\n", line_ending)
    formatted = sanitize_for_cp949(formatted)
    with open(path, "w", encoding="cp949", errors="strict", newline="") as f:
        f.write(formatted)


def safe_prepare_patch(filename, content, patches):
    """각 패치 블록이 정확히 1곳에 매칭되는지(또는 이미 적용되었는지) 정밀 검증 후 결과 텍스트 반환"""
    updated = content
    all_already = True

    for idx, (desc, old_block, new_block) in enumerate(patches, start=1):
        old_re = build_normalized_Confirm_regex(old_block)
        new_re = build_normalized_Confirm_regex(new_block)
        old_matches = list(old_re.finditer(updated))
        if len(old_matches) == 1:
            all_already = False
            updated = old_re.sub(lambda m: new_block, updated, count=1)
            print(f"  [검증 통과] {filename} #{idx} ({desc})")
        elif len(old_matches) == 0:
            new_matches = list(new_re.finditer(updated))
            if len(new_matches) >= 1:
                print(f"  [이미 적용됨] {filename} #{idx} ({desc})")
            else:
                raise RuntimeError(
                    f"[{filename}] 패치 #{idx} ({desc}) 원본 코드 블록을 찾을 수 없습니다 (매칭 0곳)."
                )
        else:
            raise RuntimeError(
                f"[{filename}] 패치 #{idx} ({desc}) 원본 코드 블록이 {len(old_matches)}곳에서 중복 발견되어 중단합니다."
            )

    return updated, all_already


# ==============================================================================
# 1. TSP_5B_DCL_and_Strut.lsp 패치 블록 정의
# ==============================================================================
PATCHES_5B = [
    (
        "tsp_brace 다이얼로그에 잭 위치(brace_jack_side) 팝업 추가",
        """      (write-line "tsp_brace : dialog { key = \\"brace_dlg_title\\"; label = \\"사보강재(Corner Strut) 세부 설정\\"; " dcl-file)
      (write-line "  : column { " dcl-file)
      (write-line "    : text { key = \\"brace_info\\"; label = \\"최대 한계 거리: -- m\\"; alignment = centered; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_name\\"; label = \\"단 이름:\\"; edit_width = 14; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_depth\\"; label = \\"설치깊이(m):\\"; edit_width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : list_box { key = \\"brace_list\\"; height = 6; width = 70; multiple_select = true; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : text { label = \\"이격 거리(m):\\"; width = 12; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_dist\\"; width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : text { label = \\"※ 1열은 코너 기준, 2열부터는 이전 열 기준 상대거리입니다.\\"; } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : text { label = \\"사보강재(Corner Strut) 규격\\"; } " dcl-file)
      (write-line "    : popup_list { key = \\"brace_spec\\"; width = 30; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_h\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_b\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tw\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"/\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tf\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : popup_list { key = \\"brace_jack\\"; label = \\"잭 종류:\\"; width = 20; } " dcl-file)
      (write-line "      : popup_list { key = \\"brace_conn\\"; label = \\"연결 방식:\\"; width = 20; } " dcl-file)
      (write-line "    } " dcl-file)""",
        """      (write-line "tsp_brace : dialog { key = \\"brace_dlg_title\\"; label = \\"사보강재(Corner Strut) 세부 설정\\"; " dcl-file)
      (write-line "  : column { " dcl-file)
      (write-line "    : text { key = \\"brace_info\\"; label = \\"최대 한계 거리: -- m\\"; alignment = centered; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_name\\"; label = \\"단 이름:\\"; edit_width = 14; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_depth\\"; label = \\"설치깊이(m):\\"; edit_width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : list_box { key = \\"brace_list\\"; height = 6; width = 70; multiple_select = true; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : text { label = \\"이격 거리(m):\\"; width = 12; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_dist\\"; width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : text { label = \\"※ 1열은 코너 기준, 2열부터는 이전 열 기준 상대거리입니다.\\"; } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : text { label = \\"사보강재(Corner Strut) 규격\\"; } " dcl-file)
      (write-line "    : popup_list { key = \\"brace_spec\\"; width = 30; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_h\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_b\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tw\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"/\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tf\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : popup_list { key = \\"brace_jack\\"; label = \\"잭 종류:\\"; width = 20; } " dcl-file)
      (write-line "      : popup_list { key = \\"brace_jack_side\\"; label = \\"잭 위치:\\"; width = 28; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : popup_list { key = \\"brace_conn\\"; label = \\"연결 방식:\\"; width = 28; } " dcl-file)
      (write-line "    } " dcl-file)""",
    ),
    (
        "tsp_brace_single 다이얼로그에 잭 위치(brace_jack_side) 팝업 추가",
        """            (write-line "tsp_brace_single : dialog { key = \\"brace_dlg_title\\"; label = \\"사보강재(Corner Strut) 세부 설정\\"; " dcl-file)
      (write-line "  : column { " dcl-file)
      (write-line "    : text { key = \\"brace_info\\"; label = \\"최대 한계 거리: -- m\\"; alignment = centered; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_name\\"; label = \\"단 이름:\\"; edit_width = 14; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_depth\\"; label = \\"설치깊이(m):\\"; edit_width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : list_box { key = \\"brace_list\\"; height = 6; width = 70; multiple_select = true; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : text { label = \\"이격 거리(m):\\"; width = 12; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_dist\\"; width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : text { label = \\"※ 1열은 코너 기준, 2열부터는 이전 열 기준 상대거리입니다.\\"; } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : text { label = \\"사보강재(Corner Strut) 규격\\"; } " dcl-file)
      (write-line "    : popup_list { key = \\"brace_spec\\"; width = 30; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_h\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_b\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tw\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"/\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tf\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : popup_list { key = \\"brace_jack\\"; label = \\"잭 종류:\\"; width = 20; } " dcl-file)
      (write-line "      : popup_list { key = \\"brace_conn\\"; label = \\"연결 방식:\\"; width = 20; } " dcl-file)
      (write-line "    } " dcl-file)""",
        """            (write-line "tsp_brace_single : dialog { key = \\"brace_dlg_title\\"; label = \\"사보강재(Corner Strut) 세부 설정\\"; " dcl-file)
      (write-line "  : column { " dcl-file)
      (write-line "    : text { key = \\"brace_info\\"; label = \\"최대 한계 거리: -- m\\"; alignment = centered; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_name\\"; label = \\"단 이름:\\"; edit_width = 14; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_stage_depth\\"; label = \\"설치깊이(m):\\"; edit_width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : list_box { key = \\"brace_list\\"; height = 6; width = 70; multiple_select = true; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : text { label = \\"이격 거리(m):\\"; width = 12; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_dist\\"; width = 10; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : text { label = \\"※ 1열은 코너 기준, 2열부터는 이전 열 기준 상대거리입니다.\\"; } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : text { label = \\"사보강재(Corner Strut) 규격\\"; } " dcl-file)
      (write-line "    : popup_list { key = \\"brace_spec\\"; width = 30; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : edit_box { key = \\"brace_h\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_b\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"x\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tw\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "      : text { label = \\"/\\"; } " dcl-file)
      (write-line "      : edit_box { key = \\"brace_tf\\"; width = 5; is_enabled = false; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : spacer { height = 0.5; } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : popup_list { key = \\"brace_jack\\"; label = \\"잭 종류:\\"; width = 20; } " dcl-file)
      (write-line "      : popup_list { key = \\"brace_jack_side\\"; label = \\"잭 위치:\\"; width = 28; } " dcl-file)
      (write-line "    } " dcl-file)
      (write-line "    : row { " dcl-file)
      (write-line "      : popup_list { key = \\"brace_conn\\"; label = \\"연결 방식:\\"; width = 28; } " dcl-file)
      (write-line "    } " dcl-file)""",
    ),
]


# ==============================================================================
# 2. TSP_5J_DCL_and_Strut.lsp 패치 블록 정의
# ==============================================================================
PATCHES_5J = [
    (
        "tsp-brace-stage-edit-dialog 지역 변수 선언에 잭 위치 제어 변수 추가",
        """(defun tsp-brace-stage-edit-dialog (dcl-path temp-seg-data limits existing-stage def-name def-depth cur-upg-chk cur-upg-spec user-upg-pts /
                                    dcl-id result seg-id-str cur-wale-spec cur-wale-vals cur-wale-h
                                    cur-idx prev-idx seg-prev seg-cur p-corner p-prev p-next a1 a2 delta internal-angle-deg
                                    hard-limit soft-limit full-brace-list std-len jack-list conn-list slender-list
                                    stage-name stage-depth-str temp-b-rows
                                    init-slender init-is-manual init-interval init-enable init-margin
                                    init-post-enable init-post-side init-post-layout init-post-invert
                                    user-input-dist user-brace-spec user-brace-jack user-brace-conn
                                    user-chk-upgrade user-upg-spec is-conn-touched initial-conn-idx last-row-conn last-conn-idx
                                    orig-seg-data did-temp-draw loop-brace ret-val save-ui-state-str
                                    get-auto-spec update-upgrade-ui update-stiffener-ui update-brace-list validate-and-close
                                    prev-wale-spec prev-wale-vals prev-wale-h adj-max-h adj-max-spec last-auto-max-spec is-init-phase
                                    input-dist num-b-rows prev-dist abs-dist is-dbl-sel min-req do-proceed tmp-id idx res-spec res-custom tmp-spec res-jack res-conn
                                    sel-idx-str sel-indices sel-idx next-b-row next-dist cur-abs-dist i new-list
                                    tmp-upg tmp-stage tmp-brace tmp-seg tmp-id-num)""",
        """(defun tsp-brace-stage-edit-dialog (dcl-path temp-seg-data limits existing-stage def-name def-depth cur-upg-chk cur-upg-spec user-upg-pts /
                                    dcl-id result seg-id-str prev-seg-id-str cur-wale-spec cur-wale-vals cur-wale-h
                                    cur-idx prev-idx seg-prev seg-cur p-corner p-prev p-next a1 a2 delta internal-angle-deg
                                    hard-limit soft-limit full-brace-list std-len jack-list jack-side-codes jack-side-labels conn-list slender-list
                                    stage-name stage-depth-str temp-b-rows
                                    init-slender init-is-manual init-interval init-enable init-margin
                                    init-post-enable init-post-side init-post-layout init-post-invert
                                    user-input-dist user-brace-spec user-brace-jack user-brace-jack-side user-brace-conn
                                    user-chk-upgrade user-upg-spec is-conn-touched initial-conn-idx last-row-conn last-conn-idx
                                    orig-seg-data did-temp-draw loop-brace ret-val save-ui-state-str
                                    get-auto-spec update-jack-side-ui sync-brace-row-to-ui update-upgrade-ui update-stiffener-ui update-brace-list validate-and-close
                                    prev-wale-spec prev-wale-vals prev-wale-h adj-max-h adj-max-spec last-auto-max-spec is-init-phase
                                    input-dist num-b-rows prev-dist abs-dist is-dbl-sel min-req do-proceed tmp-id idx res-spec res-custom tmp-spec res-jack res-jack-side res-conn
                                    sel-idx-str sel-indices sel-idx next-b-row next-dist cur-abs-dist i new-list
                                    tmp-upg tmp-stage tmp-brace tmp-seg tmp-id-num)""",
    ),
    (
        "이전 세그먼트 번호 문자열(prev-seg-id-str) 초기화",
        """  (setq seg-id-str (itoa (if (assoc 'ID temp-seg-data) (cdr (assoc 'ID temp-seg-data)) 0)))
  (setq cur-wale-spec (cdr (assoc 'WALE-SPEC temp-seg-data)))
  (setq cur-wale-vals (if (= cur-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM temp-seg-data)) (parse-h-spec cur-wale-spec)))
  (setq cur-wale-h (if cur-wale-vals (nth 0 cur-wale-vals) 300))

  (setq cur-idx (1- (if (assoc 'ID temp-seg-data) (cdr (assoc 'ID temp-seg-data)) 1)))
  (setq prev-idx (if (= cur-idx 0) (1- (length *segment-list*)) (1- cur-idx)))
  (setq seg-prev (if (and (>= prev-idx 0) (< prev-idx (length *segment-list*))) (nth prev-idx *segment-list*) nil))
  (setq seg-cur temp-seg-data)""",
        """  (setq seg-id-str (itoa (if (assoc 'ID temp-seg-data) (cdr (assoc 'ID temp-seg-data)) 0)))
  (setq cur-wale-spec (cdr (assoc 'WALE-SPEC temp-seg-data)))
  (setq cur-wale-vals (if (= cur-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM temp-seg-data)) (parse-h-spec cur-wale-spec)))
  (setq cur-wale-h (if cur-wale-vals (nth 0 cur-wale-vals) 300))

  (setq cur-idx (1- (if (assoc 'ID temp-seg-data) (cdr (assoc 'ID temp-seg-data)) 1)))
  (setq prev-idx (if (= cur-idx 0) (1- (length *segment-list*)) (1- cur-idx)))
  (setq seg-prev (if (and (>= prev-idx 0) (< prev-idx (length *segment-list*))) (nth prev-idx *segment-list*) nil))
  (setq prev-seg-id-str (itoa (if (and seg-prev (assoc 'ID seg-prev)) (cdr (assoc 'ID seg-prev)) (1+ prev-idx))))
  (setq seg-cur temp-seg-data)""",
    ),
    (
        "UI 상태 저장 변수(user-brace-jack-side) 초기화",
        """  (setq user-input-dist nil user-brace-spec nil user-brace-jack nil user-brace-conn nil)""",
        """  (setq user-input-dist nil user-brace-spec nil user-brace-jack nil user-brace-jack-side nil user-brace-conn nil)""",
    ),
    (
        "save-ui-state-str 및 brace_jack_side 팝업 목록 구성",
        """        (setq save-ui-state-str
          "(progn
             (setq stage-name (get_tile \\"brace_stage_name\\"))
             (setq stage-depth-str (get_tile \\"brace_stage_depth\\"))
             (setq user-input-dist (get_tile \\"brace_dist\\"))
             (setq user-brace-spec (get_tile \\"brace_spec\\"))
             (setq user-brace-jack (get_tile \\"brace_jack\\"))
             (setq user-brace-conn (get_tile \\"brace_conn\\"))
             (setq user-chk-upgrade (get_tile \\"chk_upgrade_wale\\"))
             (setq user-upg-spec (get_tile \\"upgrade_wale_spec\\"))
             (setq init-slender (get_tile \\"slenderness_limit\\"))
             (setq init-is-manual (get_tile \\"chk_manual_interval\\"))
             (setq init-interval (get_tile \\"stiffener_interval\\"))
             (setq init-enable (get_tile \\"chk_stiffener_enable\\"))
             (setq init-margin (get_tile \\"stiffener_margin\\"))
             (setq init-post-enable (get_tile \\"chk_post_enable\\"))
             (setq init-post-side (if (= (get_tile \\"post_side_R\\") \\"1\\") \\"R\\" \\"L\\"))
             (setq init-post-layout (get_tile \\"post_layout_mode\\"))
             (setq init-post-invert (get_tile \\"chk_post_invert\\"))
           )"
        )

        (set_tile "brace_dlg_title" (strcat seg-id-str "번 모서리 사보강재 세부 설정 (" stage-name ")"))
        (set_tile "brace_info" (strcat "▶ 절대 한계: " (rtos hard-limit 2 2) "m / 권장(50%): " (rtos soft-limit 2 2) "m"))
        (set_tile "brace_stage_name" stage-name)
        (set_tile "brace_stage_depth" stage-depth-str)

        ;; 세부 설정창에서는 하단 [다음] 버튼을 숨기고 [완료(확인)] / [취소]에 집중
        (mode_tile "accept" 1)

        (start_list "brace_spec")
        (mapcar 'add_list (append full-brace-list '("User-defined")))
        (end_list)
        (setq std-len (length full-brace-list))
        (setq jack-list '("잭 없음" "스크류잭" "유압잭"))
        (start_list "brace_jack") (mapcar 'add_list jack-list) (end_list)
        (setq conn-list '("" "화타쐐기 없음" "화타쐐기 TYPE-G1"))
        (start_list "brace_conn") (mapcar 'add_list conn-list) (end_list)""",
        """        (setq save-ui-state-str
          "(progn
             (setq stage-name (get_tile \\"brace_stage_name\\"))
             (setq stage-depth-str (get_tile \\"brace_stage_depth\\"))
             (setq user-input-dist (get_tile \\"brace_dist\\"))
             (setq user-brace-spec (get_tile \\"brace_spec\\"))
             (setq user-brace-jack (get_tile \\"brace_jack\\"))
             (setq user-brace-jack-side (get_tile \\"brace_jack_side\\"))
             (setq user-brace-conn (get_tile \\"brace_conn\\"))
             (setq user-chk-upgrade (get_tile \\"chk_upgrade_wale\\"))
             (setq user-upg-spec (get_tile \\"upgrade_wale_spec\\"))
             (setq init-slender (get_tile \\"slenderness_limit\\"))
             (setq init-is-manual (get_tile \\"chk_manual_interval\\"))
             (setq init-interval (get_tile \\"stiffener_interval\\"))
             (setq init-enable (get_tile \\"chk_stiffener_enable\\"))
             (setq init-margin (get_tile \\"stiffener_margin\\"))
             (setq init-post-enable (get_tile \\"chk_post_enable\\"))
             (setq init-post-side (if (= (get_tile \\"post_side_R\\") \\"1\\") \\"R\\" \\"L\\"))
             (setq init-post-layout (get_tile \\"post_layout_mode\\"))
             (setq init-post-invert (get_tile \\"chk_post_invert\\"))
           )"
        )

        (set_tile "brace_dlg_title" (strcat seg-id-str "번 모서리 사보강재 세부 설정 (" stage-name ")"))
        (set_tile "brace_info" (strcat "▶ 절대 한계: " (rtos hard-limit 2 2) "m / 권장(50%): " (rtos soft-limit 2 2) "m"))
        (set_tile "brace_stage_name" stage-name)
        (set_tile "brace_stage_depth" stage-depth-str)

        ;; 세부 설정창에서는 하단 [다음] 버튼을 숨기고 [완료(확인)] / [취소]에 집중
        (mode_tile "accept" 1)

        (start_list "brace_spec")
        (mapcar 'add_list (append full-brace-list '("User-defined")))
        (end_list)
        (setq std-len (length full-brace-list))
        (setq jack-list '("잭 없음" "스크류잭" "유압잭"))
        (start_list "brace_jack") (mapcar 'add_list jack-list) (end_list)
        ;; 사보강재 한쪽 단부 잭 설치 위치 옵션 (AUTO=기본교차, A=이전 세그먼트 측, B=현재 세그먼트 측, REV=2H 교차반전)
        (setq jack-side-codes '("AUTO" "A" "B" "REV"))
        (setq jack-side-labels
          (list
            "자동 교차 (기본)"
            (strcat prev-seg-id-str "번 구간 측 띠장")
            (strcat seg-id-str "번 구간 측 띠장")
            "2H 교차 반전"
          )
        )
        (start_list "brace_jack_side") (mapcar 'add_list jack-side-labels) (end_list)
        (setq conn-list '("" "화타쐐기 없음" "화타쐐기 TYPE-G1"))
        (start_list "brace_conn") (mapcar 'add_list conn-list) (end_list)""",
    ),
    (
        "목록 표시, 잭 위치 활성/비활성 제어, 목록 클릭 동기화 및 추가/수정 저장 로직 확장",
        """        (defun update-brace-list (/ i prev item spec m-dist rel-dist disp tag-str b-jack b-conn prev-conn last-conn-idx last-row-conn)
          (start_list "brace_list")
          (setq i 0 prev 0.0 prev-conn "화타쐐기 없음")
          (foreach item temp-b-rows
            (setq spec (car item) m-dist (nth 2 item) b-jack (nth 3 item) b-conn (nth 4 item))
            (if (or (null b-jack) (= b-jack ""))
              (setq b-jack (if (= i 0) "잭 없음" "스크류잭"))
            )
            (if (null b-conn) (setq b-conn prev-conn))
            (setq prev-conn b-conn)
            (setq rel-dist (- m-dist prev))
            (if (> m-dist hard-limit)
              (setq tag-str "  [!] 절대 한계 초과")
              (if (> m-dist soft-limit) (setq tag-str "  [!] 권장 한계 초과") (setq tag-str ""))
            )
            (if (= i 0)
              (setq disp (strcat (itoa (1+ i)) "열: " spec " (" b-jack ") [" b-conn "] / " (rtos m-dist 2 2) "m" tag-str))
              (setq disp (strcat (itoa (1+ i)) "열: " spec " (" b-jack ") [" b-conn "] / +" (rtos rel-dist 2 2) "m (누적 " (rtos m-dist 2 2) "m)" tag-str))
            )
            (add_list disp)
            (setq prev m-dist i (1+ i))
          )
          (end_list)
          (if (> (length temp-b-rows) 0)
            (set_tile "brace_list" (itoa (1- (length temp-b-rows))))
            (set_tile "brace_list" "")
          )
          (update-stiffener-ui)
          (if (= (length temp-b-rows) 0)
            (progn
              (set_tile "brace_conn" "1")
              (setq is-conn-touched nil)
            )
            (if (not is-conn-touched)
              (progn
                (setq last-row-conn (nth 4 (last temp-b-rows)))
                (setq last-conn-idx (if last-row-conn (vl-position (if (= last-row-conn "마감철판 직결") "" last-row-conn) conn-list) 1))
                (if (null last-conn-idx) (setq last-conn-idx 1))
                (set_tile "brace_conn" (itoa last-conn-idx))
              )
            )
          )
        )

        (update-brace-list)

        (if user-input-dist
          (set_tile "brace_dist" user-input-dist)
          (if (>= hard-limit 1.5) (set_tile "brace_dist" "1.5") (set_tile "brace_dist" "1.0"))
        )
        (set_tile "brace_spec" (if user-brace-spec user-brace-spec "2"))
        ;; 1열 추가 대기 상태(목록 0개)이면 기본값 "잭 없음"("0"), 2열 이상 추가 대기 상태이면 기본값 "스크류잭"("1")
        (set_tile "brace_jack"
          (if user-brace-jack
            user-brace-jack
            (if (= (length temp-b-rows) 0) "0" "1")
          )
        )

        (setq initial-conn-idx "1")
        (if (> (length temp-b-rows) 0)
          (progn
            (setq last-row-conn (nth 4 (last temp-b-rows)))
            (setq last-conn-idx (if last-row-conn (vl-position (if (= last-row-conn "마감철판 직결") "" last-row-conn) conn-list) 1))
            (if (null last-conn-idx) (setq last-conn-idx 1))
            (setq initial-conn-idx (itoa last-conn-idx))
          )
        )
        (set_tile "brace_conn" (if user-brace-conn user-brace-conn initial-conn-idx))

        (mode_tile "brace_h" 1) (mode_tile "brace_b" 1) (mode_tile "brace_tw" 1) (mode_tile "brace_tf" 1)
        (action_tile "brace_spec"
          (strcat
            "(if (= (atoi $value) std-len) "
            "  (progn (mode_tile \\"brace_h\\" 0)(mode_tile \\"brace_b\\" 0)(mode_tile \\"brace_tw\\" 0)(mode_tile \\"brace_tf\\" 0)) "
            "  (progn (mode_tile \\"brace_h\\" 1)(mode_tile \\"brace_b\\" 1)(mode_tile \\"brace_tw\\" 1)(mode_tile \\"brace_tf\\" 1))) "
            "(if (wcmatch (nth (atoi $value) full-brace-list) \\"2H *\\") "
            "  (if (< (atof (get_tile \\"brace_dist\\")) 2.0) (set_tile \\"brace_dist\\" \\"2.0\\"))) "
            "(update-upgrade-ui)"
          )
        )
        (action_tile "brace_h" "(update-upgrade-ui)")
        (action_tile "brace_conn" "(setq is-conn-touched T)")
        (action_tile "chk_stiffener_enable" "(update-stiffener-ui)")
        (action_tile "slenderness_limit" "(update-stiffener-ui)")
        (action_tile "chk_manual_interval" "(update-stiffener-ui)")

        (setq is-init-phase T)
        (update-upgrade-ui)
        (setq is-init-phase nil)

        (action_tile "btn_measure" (strcat "(progn " save-ui-state-str "(done_dialog 5))"))

        (action_tile "btn_add"
          (strcat
            "(progn (setq input-dist (atof (get_tile \\"brace_dist\\"))) (setq num-b-rows (length temp-b-rows)) (setq prev-dist (if (> num-b-rows 0) (nth 2 (last temp-b-rows)) 0.0)) (setq abs-dist (+ prev-dist input-dist)) "
            "(setq is-dbl-sel (wcmatch (if (< (atoi (get_tile \\"brace_spec\\")) std-len) (nth (atoi (get_tile \\"brace_spec\\")) full-brace-list) \\"\\") \\"2H *\\")) "
            "(setq min-req (if is-dbl-sel (if (= num-b-rows 0) 1.5 1.2) (if (= num-b-rows 0) 1.0 0.8))) "
            "(if (< input-dist min-req) "
            "  (if is-dbl-sel "
            "    (if (= num-b-rows 0) (alert (strcat \\"맞버팀대(2H) 1열은 코너부 간섭을 피하기 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"맞버팀대(2H) 다중열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "    (if (= num-b-rows 0) (alert (strcat \\"1열 사보강재(Corner Strut)는 여유 공간을 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"여러 열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "  ) "
            "(progn (setq do-proceed T) (if (> abs-dist hard-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_hard_warn\\" tmp-id) (progn (if (< hard-limit min-req) (progn (set_tile \\"warn_msg1\\" (strcat \\"물리적 여유 공간(\\" (rtos hard-limit 2 2) \\"m)이 최소 기준(\\" (rtos min-req 2 1) \\"m) 부족입니다.\\")) (set_tile \\"warn_msg2\\" \\"시공 불가 예상 구간이나, 이대로 강제 돌파(적용)하시겠습니까?\\"))) (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)) "
            "(if (> abs-dist soft-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_warn\\" tmp-id) (progn (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)))) "
            "(if do-proceed (progn (setq idx (atoi (get_tile \\"brace_spec\\"))) (if (< idx std-len) (progn (setq res-spec (nth idx full-brace-list)) (setq res-custom (parse-h-spec (vl-string-subst \\"H \\" \\"2H \\" res-spec)))) (progn (setq res-spec \\"User-defined\\") (setq res-custom (list (atoi (get_tile \\"brace_h\\")) (atoi (get_tile \\"brace_b\\")) (atoi (get_tile \\"brace_tw\\")) (atoi (get_tile \\"brace_tf\\")))))) "
            "(setq tmp-spec (get-auto-spec res-spec res-custom abs-dist nil)) "
            "(if (and (wcmatch tmp-spec \\"2H *\\") (not (wcmatch res-spec \\"2H *\\")) (< input-dist 2.0)) "
            "  (progn "
            "    (setq input-dist 2.0) "
            "    (setq abs-dist (+ prev-dist input-dist)) "
            "    (set_tile \\"brace_dist\\" \\"2.0\\") "
            "    (alert \\"세장비 허용 한계를 초과하여 맞버팀대(2H)로 자동 전환되었으며, 공간 확보를 위해 이격거리가 2.0m로 자동 보정되었습니다.\\") "
            "  ) "
            ") "
            "(setq res-spec tmp-spec) "
            "(setq res-jack (nth (atoi (get_tile \\"brace_jack\\")) jack-list)) (setq res-conn (nth (atoi (get_tile \\"brace_conn\\")) conn-list)) (setq temp-b-rows (append temp-b-rows (list (list res-spec res-custom abs-dist res-jack res-conn)))) (update-brace-list) (update-upgrade-ui) (set_tile \\"brace_jack\\" \\"1\\") "
            save-ui-state-str
            "(done_dialog 7))))))"
          )
        )

        (action_tile "btn_mod"
          (strcat
            "(progn (setq *tsp-warned-flag* nil *tsp-brace-buckle-warned* nil) (setq sel-idx-str (get_tile \\"brace_list\\")) (setq sel-indices (if (and sel-idx-str (/= sel-idx-str \\"\\")) (tsp-parse-space-string sel-idx-str) nil)) "
            "(if sel-indices (progn "
            "(setq idx (atoi (get_tile \\"brace_spec\\"))) (if (< idx std-len) (progn (setq res-spec (nth idx full-brace-list)) (setq res-custom (parse-h-spec (vl-string-subst \\"H \\" \\"2H \\" res-spec)))) (progn (setq res-spec \\"User-defined\\") (setq res-custom (list (atoi (get_tile \\"brace_h\\")) (atoi (get_tile \\"brace_b\\")) (atoi (get_tile \\"brace_tw\\")) (atoi (get_tile \\"brace_tf\\")))))) "
            "(setq res-jack (nth (atoi (get_tile \\"brace_jack\\")) jack-list)) (setq res-conn (nth (atoi (get_tile \\"brace_conn\\")) conn-list)) "
            "(if (= (length sel-indices) 1) (progn "
            "(setq sel-idx (car sel-indices)) (setq input-dist (atof (get_tile \\"brace_dist\\"))) (setq prev-dist (if (> sel-idx 0) (nth 2 (nth (1- sel-idx) temp-b-rows)) 0.0)) (setq abs-dist (+ prev-dist input-dist)) "
            "(setq is-dbl-sel (wcmatch res-spec \\"2H *\\")) "
            "(setq min-req (if is-dbl-sel (if (= sel-idx 0) 1.5 1.2) (if (= sel-idx 0) 1.0 0.8))) "
            "(setq next-b-row (if (< (1+ sel-idx) (length temp-b-rows)) (nth (1+ sel-idx) temp-b-rows) nil)) (setq next-dist (if next-b-row (nth 2 next-b-row) nil)) "
            "(if (< input-dist min-req) "
            "  (if is-dbl-sel "
            "    (if (= sel-idx 0) (alert (strcat \\"맞버팀대(2H) 1열은 코너부 간섭을 피하기 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"맞버팀대(2H) 다중열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "    (if (= sel-idx 0) (alert (strcat \\"1열 사보강재(Corner Strut)는 여유 공간을 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"여러 열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "  ) "
            "(if (and next-dist (< (- next-dist abs-dist) 0.8)) (alert \\"수정 시 다음 열과의 간격이 0.8m 미만이 되어 간섭이 발생합니다. 다음 열을 먼저 수정하거나 거리를 조절하세요.\\") (progn (setq do-proceed T) (if (> abs-dist hard-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_hard_warn\\" tmp-id) (progn (if (< hard-limit min-req) (progn (set_tile \\"warn_msg1\\" (strcat \\"물리적 여유 공간(\\" (rtos hard-limit 2 2) \\"m)이 최소 기준(\\" (rtos min-req 2 1) \\"m) 부족입니다.\\")) (set_tile \\"warn_msg2\\" \\"시공 불가 예상 구간이나, 이대로 강제 돌파(적용)하시겠습니까?\\"))) (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)) "
            "(if (> abs-dist soft-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_warn\\" tmp-id) (progn (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)))) "
            "(if do-proceed (progn (setq tmp-spec (get-auto-spec res-spec res-custom abs-dist T)) (setq i 0 new-list '()) (foreach item temp-b-rows (if (= i sel-idx) (setq new-list (append new-list (list (list tmp-spec res-custom abs-dist res-jack res-conn)))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-b-rows new-list) (update-brace-list) (update-upgrade-ui) "
            save-ui-state-str
            "(done_dialog 7))))))) "
            "(progn (setq i 0 new-list '()) (foreach item temp-b-rows (if (vl-position i sel-indices) (progn (setq cur-abs-dist (nth 2 item)) (setq tmp-spec (get-auto-spec res-spec res-custom cur-abs-dist T)) (setq new-list (append new-list (list (list tmp-spec res-custom cur-abs-dist res-jack res-conn))))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-b-rows new-list) (update-brace-list) (update-upgrade-ui) "
            save-ui-state-str
            "(done_dialog 7))) "
            ") (alert \\"수정할 항목을 선택하세요!\\")))"
          )
        )""",
        """        ;; '잭 없음'일 때 잭 위치 선택창을 비활성화하고, 스크류잭/유압잭일 때만 활성화하는 보조 함수
        (defun update-jack-side-ui ( / j-idx )
          (setq j-idx (atoi (get_tile "brace_jack")))
          (if (= j-idx 0)
            (mode_tile "brace_jack_side" 1)
            (mode_tile "brace_jack_side" 0)
          )
        )

        ;; 목록에서 사보강재 열을 클릭했을 때 해당 열의 제원/잭종류/잭위치/연결방식을 입력칸으로 불러오는 함수
        (defun sync-brace-row-to-ui (sel-str / s-list r-idx item prev-d rel-d sp cust j-str c-str j-side sp-idx j-idx js-idx c-idx)
          (setq s-list (if (and sel-str (/= sel-str "")) (tsp-parse-space-string sel-str) nil))
          (if (and s-list (= (length s-list) 1))
            (progn
              (setq r-idx (car s-list))
              (if (and (>= r-idx 0) (< r-idx (length temp-b-rows)))
                (progn
                  (setq item (nth r-idx temp-b-rows))
                  (setq prev-d (if (> r-idx 0) (nth 2 (nth (1- r-idx) temp-b-rows)) 0.0))
                  (setq rel-d (- (nth 2 item) prev-d))
                  (set_tile "brace_dist" (rtos rel-d 2 2))

                  (setq sp (nth 0 item) cust (nth 1 item))
                  (setq sp-idx (vl-position sp full-brace-list))
                  (if sp-idx
                    (progn
                      (set_tile "brace_spec" (itoa sp-idx))
                      (mode_tile "brace_h" 1) (mode_tile "brace_b" 1) (mode_tile "brace_tw" 1) (mode_tile "brace_tf" 1)
                    )
                    (progn
                      (set_tile "brace_spec" (itoa std-len))
                      (mode_tile "brace_h" 0) (mode_tile "brace_b" 0) (mode_tile "brace_tw" 0) (mode_tile "brace_tf" 0)
                      (if (and cust (= (length cust) 4))
                        (progn
                          (set_tile "brace_h" (itoa (nth 0 cust)))
                          (set_tile "brace_b" (itoa (nth 1 cust)))
                          (set_tile "brace_tw" (itoa (nth 2 cust)))
                          (set_tile "brace_tf" (itoa (nth 3 cust)))
                        )
                      )
                    )
                  )

                  (setq j-str (nth 3 item))
                  (if (or (null j-str) (= j-str ""))
                    (setq j-str (if (= r-idx 0) "잭 없음" "스크류잭"))
                  )
                  (setq j-idx (vl-position j-str jack-list))
                  (set_tile "brace_jack" (itoa (if j-idx j-idx 1)))

                  (setq j-side (if (> (length item) 5) (nth 5 item) "AUTO"))
                  (if (or (null j-side) (= j-side "")) (setq j-side "AUTO"))
                  (setq js-idx (vl-position j-side jack-side-codes))
                  (set_tile "brace_jack_side" (itoa (if js-idx js-idx 0)))
                  (update-jack-side-ui)

                  (setq c-str (nth 4 item))
                  (setq c-idx (if c-str (vl-position (if (= c-str "마감철판 직결") "" c-str) conn-list) 1))
                  (if (null c-idx) (setq c-idx 1))
                  (set_tile "brace_conn" (itoa c-idx))
                  (update-upgrade-ui)
                )
              )
            )
          )
        )

        (defun update-brace-list (/ i prev item spec m-dist rel-dist disp tag-str b-jack b-jack-side is-2h-row eff-side side-disp jack-disp b-conn prev-conn last-conn-idx last-row-conn)
          (start_list "brace_list")
          (setq i 0 prev 0.0 prev-conn "화타쐐기 없음")
          (foreach item temp-b-rows
            (setq spec (car item) m-dist (nth 2 item) b-jack (nth 3 item) b-conn (nth 4 item))
            (setq b-jack-side (if (> (length item) 5) (nth 5 item) "AUTO"))
            (if (or (null b-jack-side) (= b-jack-side "")) (setq b-jack-side "AUTO"))
            (if (or (null b-jack) (= b-jack ""))
              (setq b-jack (if (= i 0) "잭 없음" "스크류잭"))
            )
            (if (null b-conn) (setq b-conn prev-conn))
            (setq prev-conn b-conn)
            (setq is-2h-row (wcmatch spec "2H *"))
            (if (= b-jack "잭 없음")
              (setq jack-disp b-jack)
              (progn
                (if is-2h-row
                  (setq side-disp
                    (cond
                      ((= b-jack-side "A") (strcat ":" prev-seg-id-str "번측"))
                      ((= b-jack-side "B") (strcat ":" seg-id-str "번측"))
                      ((= b-jack-side "REV") ":교차반전")
                      (t ":교차")
                    )
                  )
                  (progn
                    (setq eff-side
                      (cond
                        ((= b-jack-side "A") "A")
                        ((= b-jack-side "B") "B")
                        ((= b-jack-side "REV") (if (= (rem i 2) 1) "B" "A"))
                        (t (if (= (rem i 2) 1) "A" "B"))
                      )
                    )
                    (setq side-disp (strcat ":" (if (= eff-side "A") prev-seg-id-str seg-id-str) "번측"))
                  )
                )
                (setq jack-disp (strcat b-jack side-disp))
              )
            )
            (setq rel-dist (- m-dist prev))
            (if (> m-dist hard-limit)
              (setq tag-str "  [!] 절대 한계 초과")
              (if (> m-dist soft-limit) (setq tag-str "  [!] 권장 한계 초과") (setq tag-str ""))
            )
            (if (= i 0)
              (setq disp (strcat (itoa (1+ i)) "열: " spec " (" jack-disp ") [" b-conn "] / " (rtos m-dist 2 2) "m" tag-str))
              (setq disp (strcat (itoa (1+ i)) "열: " spec " (" jack-disp ") [" b-conn "] / +" (rtos rel-dist 2 2) "m (누적 " (rtos m-dist 2 2) "m)" tag-str))
            )
            (add_list disp)
            (setq prev m-dist i (1+ i))
          )
          (end_list)
          (if (> (length temp-b-rows) 0)
            (set_tile "brace_list" (itoa (1- (length temp-b-rows))))
            (set_tile "brace_list" "")
          )
          (update-stiffener-ui)
          (if (= (length temp-b-rows) 0)
            (progn
              (set_tile "brace_conn" "1")
              (setq is-conn-touched nil)
            )
            (if (not is-conn-touched)
              (progn
                (setq last-row-conn (nth 4 (last temp-b-rows)))
                (setq last-conn-idx (if last-row-conn (vl-position (if (= last-row-conn "마감철판 직결") "" last-row-conn) conn-list) 1))
                (if (null last-conn-idx) (setq last-conn-idx 1))
                (set_tile "brace_conn" (itoa last-conn-idx))
              )
            )
          )
        )

        (update-brace-list)

        (if user-input-dist
          (set_tile "brace_dist" user-input-dist)
          (if (>= hard-limit 1.5) (set_tile "brace_dist" "1.5") (set_tile "brace_dist" "1.0"))
        )
        (set_tile "brace_spec" (if user-brace-spec user-brace-spec "2"))
        ;; 1열 추가 대기 상태(목록 0개)이면 기본값 "잭 없음"("0"), 2열 이상 추가 대기 상태이면 기본값 "스크류잭"("1")
        (set_tile "brace_jack"
          (if user-brace-jack
            user-brace-jack
            (if (= (length temp-b-rows) 0) "0" "1")
          )
        )
        (set_tile "brace_jack_side" (if user-brace-jack-side user-brace-jack-side "0"))
        (update-jack-side-ui)

        (setq initial-conn-idx "1")
        (if (> (length temp-b-rows) 0)
          (progn
            (setq last-row-conn (nth 4 (last temp-b-rows)))
            (setq last-conn-idx (if last-row-conn (vl-position (if (= last-row-conn "마감철판 직결") "" last-row-conn) conn-list) 1))
            (if (null last-conn-idx) (setq last-conn-idx 1))
            (setq initial-conn-idx (itoa last-conn-idx))
          )
        )
        (set_tile "brace_conn" (if user-brace-conn user-brace-conn initial-conn-idx))

        (mode_tile "brace_h" 1) (mode_tile "brace_b" 1) (mode_tile "brace_tw" 1) (mode_tile "brace_tf" 1)
        (action_tile "brace_list" "(sync-brace-row-to-ui $value)")
        (action_tile "brace_spec"
          (strcat
            "(if (= (atoi $value) std-len) "
            "  (progn (mode_tile \\"brace_h\\" 0)(mode_tile \\"brace_b\\" 0)(mode_tile \\"brace_tw\\" 0)(mode_tile \\"brace_tf\\" 0)) "
            "  (progn (mode_tile \\"brace_h\\" 1)(mode_tile \\"brace_b\\" 1)(mode_tile \\"brace_tw\\" 1)(mode_tile \\"brace_tf\\" 1))) "
            "(if (wcmatch (nth (atoi $value) full-brace-list) \\"2H *\\") "
            "  (if (< (atof (get_tile \\"brace_dist\\")) 2.0) (set_tile \\"brace_dist\\" \\"2.0\\"))) "
            "(update-upgrade-ui)"
          )
        )
        (action_tile "brace_h" "(update-upgrade-ui)")
        (action_tile "brace_jack" "(update-jack-side-ui)")
        (action_tile "brace_conn" "(setq is-conn-touched T)")
        (action_tile "chk_stiffener_enable" "(update-stiffener-ui)")
        (action_tile "slenderness_limit" "(update-stiffener-ui)")
        (action_tile "chk_manual_interval" "(update-stiffener-ui)")

        (setq is-init-phase T)
        (update-upgrade-ui)
        (setq is-init-phase nil)

        (action_tile "btn_measure" (strcat "(progn " save-ui-state-str "(done_dialog 5))"))

        (action_tile "btn_add"
          (strcat
            "(progn (setq input-dist (atof (get_tile \\"brace_dist\\"))) (setq num-b-rows (length temp-b-rows)) (setq prev-dist (if (> num-b-rows 0) (nth 2 (last temp-b-rows)) 0.0)) (setq abs-dist (+ prev-dist input-dist)) "
            "(setq is-dbl-sel (wcmatch (if (< (atoi (get_tile \\"brace_spec\\")) std-len) (nth (atoi (get_tile \\"brace_spec\\")) full-brace-list) \\"\\") \\"2H *\\")) "
            "(setq min-req (if is-dbl-sel (if (= num-b-rows 0) 1.5 1.2) (if (= num-b-rows 0) 1.0 0.8))) "
            "(if (< input-dist min-req) "
            "  (if is-dbl-sel "
            "    (if (= num-b-rows 0) (alert (strcat \\"맞버팀대(2H) 1열은 코너부 간섭을 피하기 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"맞버팀대(2H) 다중열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "    (if (= num-b-rows 0) (alert (strcat \\"1열 사보강재(Corner Strut)는 여유 공간을 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"여러 열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "  ) "
            "(progn (setq do-proceed T) (if (> abs-dist hard-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_hard_warn\\" tmp-id) (progn (if (< hard-limit min-req) (progn (set_tile \\"warn_msg1\\" (strcat \\"물리적 여유 공간(\\" (rtos hard-limit 2 2) \\"m)이 최소 기준(\\" (rtos min-req 2 1) \\"m) 부족입니다.\\")) (set_tile \\"warn_msg2\\" \\"시공 불가 예상 구간이나, 이대로 강제 돌파(적용)하시겠습니까?\\"))) (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)) "
            "(if (> abs-dist soft-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_warn\\" tmp-id) (progn (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)))) "
            "(if do-proceed (progn (setq idx (atoi (get_tile \\"brace_spec\\"))) (if (< idx std-len) (progn (setq res-spec (nth idx full-brace-list)) (setq res-custom (parse-h-spec (vl-string-subst \\"H \\" \\"2H \\" res-spec)))) (progn (setq res-spec \\"User-defined\\") (setq res-custom (list (atoi (get_tile \\"brace_h\\")) (atoi (get_tile \\"brace_b\\")) (atoi (get_tile \\"brace_tw\\")) (atoi (get_tile \\"brace_tf\\")))))) "
            "(setq tmp-spec (get-auto-spec res-spec res-custom abs-dist nil)) "
            "(if (and (wcmatch tmp-spec \\"2H *\\") (not (wcmatch res-spec \\"2H *\\")) (< input-dist 2.0)) "
            "  (progn "
            "    (setq input-dist 2.0) "
            "    (setq abs-dist (+ prev-dist input-dist)) "
            "    (set_tile \\"brace_dist\\" \\"2.0\\") "
            "    (alert \\"세장비 허용 한계를 초과하여 맞버팀대(2H)로 자동 전환되었으며, 공간 확보를 위해 이격거리가 2.0m로 자동 보정되었습니다.\\") "
            "  ) "
            ") "
            "(setq res-spec tmp-spec) "
            "(setq res-jack (nth (atoi (get_tile \\"brace_jack\\")) jack-list)) (setq res-jack-side (nth (atoi (get_tile \\"brace_jack_side\\")) jack-side-codes)) (setq res-conn (nth (atoi (get_tile \\"brace_conn\\")) conn-list)) (setq temp-b-rows (append temp-b-rows (list (list res-spec res-custom abs-dist res-jack res-conn res-jack-side)))) (update-brace-list) (update-upgrade-ui) (set_tile \\"brace_jack\\" \\"1\\") (set_tile \\"brace_jack_side\\" \\"0\\") (update-jack-side-ui) "
            save-ui-state-str
            "(done_dialog 7))))))"
          )
        )

        (action_tile "btn_mod"
          (strcat
            "(progn (setq *tsp-warned-flag* nil *tsp-brace-buckle-warned* nil) (setq sel-idx-str (get_tile \\"brace_list\\")) (setq sel-indices (if (and sel-idx-str (/= sel-idx-str \\"\\")) (tsp-parse-space-string sel-idx-str) nil)) "
            "(if sel-indices (progn "
            "(setq idx (atoi (get_tile \\"brace_spec\\"))) (if (< idx std-len) (progn (setq res-spec (nth idx full-brace-list)) (setq res-custom (parse-h-spec (vl-string-subst \\"H \\" \\"2H \\" res-spec)))) (progn (setq res-spec \\"User-defined\\") (setq res-custom (list (atoi (get_tile \\"brace_h\\")) (atoi (get_tile \\"brace_b\\")) (atoi (get_tile \\"brace_tw\\")) (atoi (get_tile \\"brace_tf\\")))))) "
            "(setq res-jack (nth (atoi (get_tile \\"brace_jack\\")) jack-list)) (setq res-jack-side (nth (atoi (get_tile \\"brace_jack_side\\")) jack-side-codes)) (setq res-conn (nth (atoi (get_tile \\"brace_conn\\")) conn-list)) "
            "(if (= (length sel-indices) 1) (progn "
            "(setq sel-idx (car sel-indices)) (setq input-dist (atof (get_tile \\"brace_dist\\"))) (setq prev-dist (if (> sel-idx 0) (nth 2 (nth (1- sel-idx) temp-b-rows)) 0.0)) (setq abs-dist (+ prev-dist input-dist)) "
            "(setq is-dbl-sel (wcmatch res-spec \\"2H *\\")) "
            "(setq min-req (if is-dbl-sel (if (= sel-idx 0) 1.5 1.2) (if (= sel-idx 0) 1.0 0.8))) "
            "(setq next-b-row (if (< (1+ sel-idx) (length temp-b-rows)) (nth (1+ sel-idx) temp-b-rows) nil)) (setq next-dist (if next-b-row (nth 2 next-b-row) nil)) "
            "(if (< input-dist min-req) "
            "  (if is-dbl-sel "
            "    (if (= sel-idx 0) (alert (strcat \\"맞버팀대(2H) 1열은 코너부 간섭을 피하기 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"맞버팀대(2H) 다중열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "    (if (= sel-idx 0) (alert (strcat \\"1열 사보강재(Corner Strut)는 여유 공간을 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\")) (alert (strcat \\"여러 열 설치 시 부재 간 간섭 방지를 위해 최소 \\" (rtos min-req 2 1) \\"m 이상 이격해야 합니다.\\"))) "
            "  ) "
            "(if (and next-dist (< (- next-dist abs-dist) 0.8)) (alert \\"수정 시 다음 열과의 간격이 0.8m 미만이 되어 간섭이 발생합니다. 다음 열을 먼저 수정하거나 거리를 조절하세요.\\") (progn (setq do-proceed T) (if (> abs-dist hard-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_hard_warn\\" tmp-id) (progn (if (< hard-limit min-req) (progn (set_tile \\"warn_msg1\\" (strcat \\"물리적 여유 공간(\\" (rtos hard-limit 2 2) \\"m)이 최소 기준(\\" (rtos min-req 2 1) \\"m) 부족입니다.\\")) (set_tile \\"warn_msg2\\" \\"시공 불가 예상 구간이나, 이대로 강제 돌파(적용)하시겠습니까?\\"))) (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)) "
            "(if (> abs-dist soft-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \\"tsp_brace_warn\\" tmp-id) (progn (action_tile \\"accept\\" \\"(done_dialog 1)\\") (action_tile \\"cancel\\" \\"(done_dialog 0)\\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)))) "
            "(if do-proceed (progn (setq tmp-spec (get-auto-spec res-spec res-custom abs-dist T)) (setq i 0 new-list '()) (foreach item temp-b-rows (if (= i sel-idx) (setq new-list (append new-list (list (list tmp-spec res-custom abs-dist res-jack res-conn res-jack-side)))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-b-rows new-list) (update-brace-list) (update-upgrade-ui) "
            save-ui-state-str
            "(done_dialog 7))))))) "
            "(progn (setq i 0 new-list '()) (foreach item temp-b-rows (if (vl-position i sel-indices) (progn (setq cur-abs-dist (nth 2 item)) (setq tmp-spec (get-auto-spec res-spec res-custom cur-abs-dist T)) (setq new-list (append new-list (list (list tmp-spec res-custom cur-abs-dist res-jack res-conn res-jack-side))))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-b-rows new-list) (update-brace-list) (update-upgrade-ui) "
            save-ui-state-str
            "(done_dialog 7))) "
            ") (alert \\"수정할 항목을 선택하세요!\\")))"
          )
        )""",
    ),
]


# ==============================================================================
# 3. TSP_4E_Plan.lsp 패치 블록 정의
# ==============================================================================
PATCHES_4E = [
    (
        "tsp-draw-all-corner-braces 지역 변수에 b-jack-side 추가",
        """(defun tsp-draw-all-corner-braces (boundary-ent boundary-orient / boundary-handle is-closed orig-vertices num-vertices max-segments idx prev-seg-idx next-seg-idx prev-def next-def prev-vertex curr-vertex next-vertex p-brace c-upg upg-vals prev-wale-spec prev-wale-vals w-h-prev next-wale-spec next-wale-vals w-h-next dir-A dir-B norm-A norm-B brace-b-rows b-row-idx b-spec b-custom b-dist-m b-jack b-conn b-dist b-h half-w""",
        """(defun tsp-draw-all-corner-braces (boundary-ent boundary-orient / boundary-handle is-closed orig-vertices num-vertices max-segments idx prev-seg-idx next-seg-idx prev-def next-def prev-vertex curr-vertex next-vertex p-brace c-upg upg-vals prev-wale-spec prev-wale-vals w-h-prev next-wale-spec next-wale-vals w-h-next dir-A dir-B norm-A norm-B brace-b-rows b-row-idx b-spec b-custom b-dist-m b-jack b-jack-side b-conn b-dist b-h half-w""",
    ),
    (
        "다단 사보강재 동일 규격/이격거리 병합 시 명시적 잭 위치 지정값 보존",
        """                    (if exist-r
                      (progn
                        (setq ex-score (+ (if (wcmatch (nth 0 exist-r) "2H *") 100000 0) (* (nth 0 (nth 1 exist-r)) 100) (nth 1 (nth 1 exist-r))))
                        (if (> r-score ex-score)
                          (setq merged-rows (subst r exist-r merged-rows))
                        )
                      )
                      (setq merged-rows (append merged-rows (list r)))
                    )""",
        """                    (if exist-r
                      (progn
                        (setq ex-score (+ (if (wcmatch (nth 0 exist-r) "2H *") 100000 0) (* (nth 0 (nth 1 exist-r)) 100) (nth 1 (nth 1 exist-r))))
                        (if (or (> r-score ex-score)
                                (and (= r-score ex-score)
                                     (or (<= (length exist-r) 5) (= (nth 5 exist-r) "AUTO") (null (nth 5 exist-r)))
                                     (> (length r) 5)
                                     (nth 5 r)
                                     (/= (nth 5 r) "AUTO")))
                          (setq merged-rows (subst r exist-r merged-rows))
                        )
                      )
                      (setq merged-rows (append merged-rows (list r)))
                    )""",
    ),
    (
        "사보강재 열별 b-jack-side(AUTO/A/B/REV) 추출 및 한쪽 단부 잭 위치(jack-side) 반영",
        """              ;;;; 사보강재 단일/맞버팀대(Double Strut) 작도 로직
              (foreach b-row brace-b-rows
                (setq b-spec (nth 0 b-row) b-custom (nth 1 b-row) b-dist-m (nth 2 b-row) b-jack (nth 3 b-row) b-conn (nth 4 b-row))
                (if (or (null b-jack) (= b-jack ""))
                  (setq b-jack (if (= b-row-idx 0) "잭 없음" "스크류잭"))
                )
                (if (null b-conn) (setq b-conn (if (= b-row-idx 0) "화타쐐기 없음" "화타쐐기 TYPE-G1")))""",
        """              ;;;; 사보강재 단일/맞버팀대(Double Strut) 작도 로직
              (foreach b-row brace-b-rows
                (setq b-spec (nth 0 b-row) b-custom (nth 1 b-row) b-dist-m (nth 2 b-row) b-jack (nth 3 b-row) b-conn (nth 4 b-row))
                (setq b-jack-side (if (> (length b-row) 5) (nth 5 b-row) "AUTO"))
                (if (or (null b-jack-side) (= b-jack-side "")) (setq b-jack-side "AUTO"))
                (if (or (null b-jack) (= b-jack ""))
                  (setq b-jack (if (= b-row-idx 0) "잭 없음" "스크류잭"))
                )
                (if (null b-conn) (setq b-conn (if (= b-row-idx 0) "화타쐐기 없음" "화타쐐기 TYPE-G1")))""",
    ),
    (
        "사용자 지정 잭 위치(AUTO/A/B/REV)에 따라 A단 또는 B단 한쪽에만 잭 배치",
        """                          ;; 맞버팀대(2H) 및 단일(1H) 지그재그 교차 배치 로직 ("잭 없음" 선택 시 jack-side = nil)
                          (if (= b-jack "잭 없음")
                            (setq jack-side nil)
                            (if is-double
                              (setq jack-side (if (= sub-idx 0) "A" "B"))
                              (setq jack-side (if (= (rem b-row-idx 2) 1) "A" "B"))
                            )
                          )""",
        """                          ;; 맞버팀대(2H) 및 단일(1H) 단부 잭 위치 결정 ("잭 없음" 선택 시 jack-side = nil, 그 외 사용자 지정 A/B/REV/AUTO 반영)
                          (if (= b-jack "잭 없음")
                            (setq jack-side nil)
                            (cond
                              ((= b-jack-side "A") (setq jack-side "A"))
                              ((= b-jack-side "B") (setq jack-side "B"))
                              ((= b-jack-side "REV")
                               (if is-double
                                 (setq jack-side (if (= sub-idx 0) "B" "A"))
                                 (setq jack-side (if (= (rem b-row-idx 2) 1) "B" "A"))
                               )
                              )
                              (t
                               (if is-double
                                 (setq jack-side (if (= sub-idx 0) "A" "B"))
                                 (setq jack-side (if (= (rem b-row-idx 2) 1) "A" "B"))
                               )
                              )
                            )
                          )""",
    ),
]


def apply_patch():
    print("=" * 72)
    print("[TSP 패치 시작] 사보강재(Corner Strut) 잭 위치 사용자 지정 기능 추가")
    print("=" * 72)

    target_specs = [
        ("TSP_5B_DCL_and_Strut.lsp", PATCHES_5B),
        ("TSP_5J_DCL_and_Strut.lsp", PATCHES_5J),
        ("TSP_4E_Plan.lsp", PATCHES_4E),
    ]

    resolved = []
    for fname, patches in target_specs:
        fpath = find_target_file(fname)
        if not fpath:
            raise FileNotFoundError(
                f"대상 파일 '{fname}'을(를) 현재/상위/하위 폴더에서 찾을 수 없습니다."
            )
        content = read_lisp_file(fpath)
        line_ending = "\r\n" if "\r\n" in content else "\n"
        updated, all_already = safe_prepare_patch(fname, content, patches)
        resolved.append((fname, fpath, updated, line_ending, all_already))

    modified_files = []
    for fname, fpath, updated, line_ending, all_already in resolved:
        if all_already:
            print(f"[건너뜀] {fname}: 이미 최신 패치가 적용되어 있습니다.")
            continue
        bak_path = fpath + ".bak"
        shutil.copy2(fpath, bak_path)
        print(f"[백업 완료] {fname} -> {bak_path}")
        write_lisp_file(fpath, updated, line_ending)
        modified_files.append(fname)
        print(f"[패치 적용 완료] {fname}")

    print("=" * 72)
    if modified_files:
        print(f"[성공] 총 {len(modified_files)}개 파일 패치 완료: {', '.join(modified_files)}")
    else:
        print("[안내] 모든 대상 파일에 이미 패치가 반영되어 있습니다.")
    print("AutoCAD에서 'TSP.lsp'를 다시 APPLOAD 하거나 (load \"TSP.lsp\")를 실행해 주세요.")
    print("=" * 72)


if __name__ == "__main__":
    try:
        apply_patch()
    except Exception as e:
        print("\n" + "!" * 72)
        print("[에러 발생] 작업 도중 문제가 발생하여 원본 파일을 변경하지 않고 중단했습니다.")
        print(f"에러 메시지: {e}")
        print("-" * 72)
        print("[상세 에러 로그 (Traceback)]")
        traceback.print_exc()
        print("!" * 72)
    finally:
        input("\n결과를 확인하셨으면 엔터(Enter) 키를 누르세요...")
