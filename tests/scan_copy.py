#!/usr/bin/env python3
"""Mechanical pass over the app's prose against ../voice/ai_tells_checklist.md.

Run from the project root:  python3 tests/scan_copy.py
It reads every string literal in app.R and R/*.R that looks like prose (not SQL,
not CSS) and reports matches for the checklist items a regex can catch. It
cannot judge rhythm, rule-of-three, or aphoristic endings; those need reading.
A hit is a prompt to look, not a verdict: 'just those three labs' is fine.
"""
import re, glob, sys
PATS = {
 "A1 opener verbs":      r"\b(delve|dive into|unpack)\b",
 "A2 prestige nouns":    r"\b(tapestry|realm|mosaic|ecosystem|symphony|labyrinth|beacon|cornerstone|testament|journey|odyssey)\b",
 "A3 inflated import.":  r"\b(crucial|pivotal|vital|paramount|essential|invaluable)\b",
 "A4 corporate adj.":    r"\b(robust|seamless|comprehensive|multifaceted|nuanced|holistic|cutting-edge|transformative|dynamic|vibrant)\b",
 "A5 inflated verbs":    r"\b(leverage|utilize|harness|streamline|facilitate|optimize|empower|foster|bolster|elevate|unlock|unleash|navigate|illuminate|showcase|garner)\w*\b",
 "A6 trailing -ing":     r", (underscoring|highlighting|emphasizing|showcasing|reflecting) ",
 "A7 excess-frequency":  r"\b(intricate|meticulous\w*|notable|compelling|profound|myriad|plethora)\b",
 "A8 signposts":         r"(worth noting|important to note|\bNotably\b|\bInterestingly\b|\bImportantly\b|\bNote that\b)",
 "A9 stock openers":     r"(In today's|In the realm of|When it comes to|At its core)",
 "A10 stock closers":    r"\b(In conclusion|In summary|Overall,|Ultimately)\b",
 "A11 connective pile":  r"\b(Furthermore|Moreover|Additionally|That being said)\b",
 "A15 promotional":      r"\b(powerful|elegant|must-know|game-changer)\b",
 "A17 minimizers":       r"\b(simply|just|easily|effortlessly)\b",
 "B18 contrastive":      r"(\b[Ii]t'?s not \w+[^.]*?, it'?s\b|isn't about|not merely|not just\b|not only\b[^.]*\bbut\b)",
 "B22 colon teasers":    r"(Here's the thing|The catch\?|The result\?)",
 "B24 think-of-it":      r"\b(Think of it as|Imagine )",
 "C35-38 chat/cheer":    r"(Great question|Great job|Congratulations|Happy \w+ing|I hope this helps|Let's|let's)",
 "C42 forced quirk":     r"\((yes, really|don't worry)",
 "C43 presumed reader":  r"(You'll notice|You might be wondering|You may be wondering)",
 "E56-62 tutorial":      r"(In this section|Now that we|Pro tip|Fun fact|Best practice:|Key takeaways|By the end of this)",
 "exclamation marks":    r"[A-Za-z]!(?=\s|$|\")",
}
SKIP = re.compile(r"^\s*(SELECT|WITH|INSERT|UPDATE|DELETE|CREATE|ALTER|DROP|BEGIN|PRAGMA|EXPLAIN|background|color|font|padding|margin|display|width|max-width|overflow|text-align|library\()", re.I)
total = 0
for f in ["app.R"] + sorted(glob.glob("R/*.R")):
    src = open(f, encoding="utf-8").read()
    src = re.sub(r"(?m)^\s*#.*$", "", src)                       # R comments are not copy
    strs = [s for s in re.findall(r'"((?:[^"\\]|\\.)*)"', src) if len(s) > 2 and not SKIP.match(s)]
    text = "\n".join(strs)
    for name, pat in PATS.items():
        for m in re.finditer(pat, text):
            total += 1
            ctx = text[max(0, m.start()-50): m.end()+40].replace("\n", " | ")
            print(f"{f:22s} {name:20s} …{ctx}…")
# em-dash density: more than one per 600 characters of prose in a file
for f in sorted(glob.glob("R/tab_*.R")):
    src = re.sub(r"(?m)^\s*#.*$", "", open(f, encoding="utf-8").read())
    strs = [s for s in re.findall(r'"((?:[^"\\]|\\.)*)"', src) if len(s) > 2 and not SKIP.match(s)]
    text = " ".join(strs); n = text.count("—")
    if n and len(text)/n < 600: print(f"{f:22s} em-dash density      {n} in {len(text)} chars")
print(f"\n{total} pattern hit(s) to read.")
