# =============================================================================
# editor_keys.R — indentation keys for every SQL editor (.sql-textarea)
#
# A browser text box uses Tab to move focus to the next control, so learners
# could not indent a stacked WHERE. This handler, added once to the page head
# in app.R, gives every editor (including ones rendered later by renderUI):
#   Tab          insert two spaces, or indent every selected line
#   Shift+Tab    remove up to two leading spaces from the current/selected lines
#   Enter        new line that keeps the current line's indentation
#   Esc, Tab     leave the editor (keyboard users must never be trapped)
# Edits go through insertText where the browser supports it, so Ctrl+Z works,
# and always end with an `input` event so Shiny sees the new text.
# =============================================================================

EDITOR_KEYS_JS <- r"---[
(function () {
  var INDENT = '  ';
  var escaped = new WeakSet();

  function replaceRange(ta, start, end, text, selStart, selEnd) {
    ta.focus();
    ta.setSelectionRange(start, end);
    var ok = false;
    try { ok = document.execCommand('insertText', false, text); } catch (e) { ok = false; }
    if (!ok || ta.value.substring(start, start + text.length) !== text) {
      ta.setRangeText(text, start, end, 'end');
    }
    ta.setSelectionRange(selStart, selEnd);
    ta.dispatchEvent(new Event('input', { bubbles: true }));
  }

  function lineBlock(ta) {
    var v = ta.value, s = ta.selectionStart, e = ta.selectionEnd;
    var from = v.lastIndexOf('\n', s - 1) + 1;
    var endAt = (e > s && v.charAt(e - 1) === '\n') ? e - 1 : e;
    var to = v.indexOf('\n', endAt);
    if (to === -1) to = v.length;
    return { from: from, to: to, text: v.substring(from, to) };
  }

  document.addEventListener('keydown', function (ev) {
    var ta = ev.target;
    if (!ta || !ta.classList || !ta.classList.contains('sql-textarea')) return;
    if (ev.isComposing || ev.ctrlKey || ev.metaKey || ev.altKey) return;

    if (ev.key === 'Escape') { escaped.add(ta); return; }
    if (ev.key === 'Tab' && escaped.has(ta)) { escaped.delete(ta); return; }
    escaped.delete(ta);

    var s = ta.selectionStart, e = ta.selectionEnd, v = ta.value;

    if (ev.key === 'Tab') {
      ev.preventDefault();
      var multi = v.substring(s, e).indexOf('\n') !== -1;
      if (!ev.shiftKey && !multi) {
        replaceRange(ta, s, e, INDENT, s + INDENT.length, s + INDENT.length);
        return;
      }
      var b = lineBlock(ta);
      var lines = b.text.split('\n');
      var firstDelta = 0, total = 0;
      var out = lines.map(function (ln, i) {
        if (!ev.shiftKey) {
          if (i === 0) firstDelta = INDENT.length;
          total += INDENT.length;
          return INDENT + ln;
        }
        var n = ln.match(/^ {0,2}/)[0].length;
        if (n === 0 && ln.charAt(0) === '\t') n = 1;
        if (i === 0) firstDelta = -n;
        total -= n;
        return ln.substring(n);
      }).join('\n');
      var ns = Math.max(b.from, s + firstDelta);
      var ne = multi ? Math.max(ns, e + total) : ns;
      replaceRange(ta, b.from, b.to, out, ns, ne);
      return;
    }

    if (ev.key === 'Enter' && !ev.shiftKey) {
      var from = v.lastIndexOf('\n', s - 1) + 1;
      var lead = v.substring(from, s).match(/^[ \t]*/)[0];
      if (!lead) return;
      ev.preventDefault();
      var ins = '\n' + lead;
      replaceRange(ta, s, e, ins, s + ins.length, s + ins.length);
    }
  });

  document.addEventListener('focusout', function (ev) {
    if (ev.target && ev.target.classList && ev.target.classList.contains('sql-textarea')) {
      escaped.delete(ev.target);
    }
  });
})();
]---"
