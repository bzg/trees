#!/usr/bin/env bb

;; Copyright (c) 2019-2025 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

;; trees.clj — Static HTML generator for decision trees.
;; Reads a config.yml and produces a self-contained index.html.
;;
;; Usage: bb trees.clj [config.yml] [output.html]
;;        Defaults: config.yml -> index.html

(require '[clojure.string :as str])

;; ---------------------------------------------------------------------------
;; YAML parser (minimal, sufficient for trees config.yml)
;; ---------------------------------------------------------------------------

(defn- count-indent [line]
  (- (count line) (count (str/triml line))))

(defn- parse-yaml-value [s]
  (let [s (str/trim s)]
    (cond
      (= s "true") true
      (= s "false") false
      (re-matches #"-?\d+" s) (parse-long s)
      (re-matches #"-?\d+\.\d+" s) (parse-double s)
      (and (str/starts-with? s "\"") (str/ends-with? s "\""))
      (subs s 1 (dec (count s)))
      (and (str/starts-with? s "'") (str/ends-with? s "'"))
      (subs s 1 (dec (count s)))
      :else s)))

(defn parse-yaml
  "Parse a YAML string into Clojure data (maps, lists, scalars).
   Handles the subset used by trees config files."
  [text]
  (let [lines (->> (str/split-lines text)
                   (remove #(re-matches #"\s*#.*" %))
                   (remove str/blank?))]
    (letfn [(parse-block [lines min-indent]
              (when (seq lines)
                (let [indent  (count-indent (first lines))
                      trimmed (str/trim (first lines))]
                  (cond
                    (< indent min-indent)          [nil lines]
                    (str/starts-with? trimmed "- ") (parse-list lines min-indent)
                    (str/includes? trimmed ":")     (parse-map lines min-indent)
                    :else [(parse-yaml-value trimmed) (rest lines)]))))
            (parse-map [lines min-indent]
              (loop [result {} remaining lines]
                (if (empty? remaining)
                  [result remaining]
                  (let [indent  (count-indent (first remaining))
                        trimmed (str/trim (first remaining))]
                    (cond
                      (< indent min-indent)          [result remaining]
                      (str/starts-with? trimmed "- ") [result remaining]
                      :else
                      (if-let [[_ k v] (re-matches #"([^:]+):\s*(.*)" trimmed)]
                        (let [k (str/trim k) v (str/trim v)]
                          (if (empty? v)
                            ;; Nested block: use the actual indent of the next
                            ;; line (handles same-level list items in YAML).
                            (let [next-lines (rest remaining)
                                  child-indent (if (seq next-lines)
                                                 (let [ni (count-indent (first next-lines))]
                                                   (if (>= ni indent) ni (inc indent)))
                                                 (inc indent))
                                  [val rest-lines] (parse-block next-lines child-indent)]
                              (recur (assoc result (keyword k) val) rest-lines))
                            (recur (assoc result (keyword k) (parse-yaml-value v))
                                   (rest remaining))))
                        [result remaining]))))))
            (parse-list [lines min-indent]
              (loop [result [] remaining lines]
                (if (empty? remaining)
                  [result remaining]
                  (let [indent  (count-indent (first remaining))
                        trimmed (str/trim (first remaining))]
                    (cond
                      (< indent min-indent) [result remaining]
                      (str/starts-with? trimmed "- ")
                      (let [after-dash     (str/trim (subs trimmed 2))
                            content-indent (+ indent 2)]
                        (if (str/includes? after-dash ":")
                          (let [[_ k v] (re-matches #"([^:]+):\s*(.*)" after-dash)
                                k (str/trim k) v (str/trim v)
                                [first-map rest-after]
                                (if (empty? v)
                                  (let [[val r] (parse-block (rest remaining) content-indent)]
                                    [{(keyword k) val} r])
                                  [{(keyword k) (parse-yaml-value v)} (rest remaining)])
                                [rest-map rest-lines] (parse-map rest-after content-indent)]
                            (recur (conj result (merge first-map rest-map)) rest-lines))
                          (if (empty? after-dash)
                            (let [[val rest-lines] (parse-block (rest remaining) content-indent)]
                              (recur (conj result val) rest-lines))
                            (recur (conj result (parse-yaml-value after-dash))
                                   (rest remaining)))))
                      :else [result remaining])))))]
      (first (parse-block lines 0)))))

;; ---------------------------------------------------------------------------
;; JSON serialization & HTML escaping
;; ---------------------------------------------------------------------------

(defn- escape-js [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "")))

(defn- escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- clj->json
  "Convert Clojure data to a JSON string."
  [x]
  (cond
    (nil? x)     "null"
    (boolean? x) (str x)
    (number? x)  (str x)
    (string? x)  (str "\"" (escape-js x) "\"")
    (keyword? x) (str "\"" (name x) "\"")
    (vector? x)      (str "[" (str/join "," (map clj->json x)) "]")
    (sequential? x)  (str "[" (str/join "," (map clj->json x)) "]")
    (map? x)     (str "{"
                      (str/join ","
                                (map (fn [[k v]]
                                       (str (clj->json (if (keyword? k) (name k) (str k)))
                                            ":" (clj->json v)))
                                     x))
                      "}")
    :else        (str "\"" (escape-js (str x)) "\"")))

;; ---------------------------------------------------------------------------
;; Markdown → HTML (applied at generation time, not in the browser)
;; ---------------------------------------------------------------------------

(defn- md->html
  "Convert a subset of Markdown to HTML: bold, italic, code, links."
  [s]
  (when s
    (-> (str s)
        (str/replace #"\*\*(.+?)\*\*" "<strong>$1</strong>")
        (str/replace #"(?<!\w)_(.+?)_(?!\w)" "<em>$1</em>")
        (str/replace #"`(.+?)`" "<code>$1</code>")
        (str/replace #"\[([^\]]+)\]\(([^)]+)\)"
                     "<a href=\"$2\" target=\"_blank\" rel=\"noopener\">$1</a>"))))

(defn- md-strip
  "Strip Markdown links to plain text: [label](url) → label.
   Used for email body where markdown isn't rendered."
  [s]
  (when s (str/replace (str s) #"\[([^\]]+)\]\([^)]+\)" "$1")))

;; ---------------------------------------------------------------------------
;; i18n
;; ---------------------------------------------------------------------------

(def ^:private i18n-strings
  {:en {:contactIntro "Contact: "      :redo "Redo"
        :toggleSummaryStyle "Toggle summary style"
        :mailToMessage "Send by email" :mailSubject "Results"
        :mailBody "Hi,\\n%s\\nThanks."}
   :fr {:contactIntro "Contact : "     :redo "Recommencer"
        :toggleSummaryStyle "Changer le style de résumé"
        :mailToMessage "Envoyer par mail" :mailSubject "Résultats"
        :mailBody "Bonjour !\\n%s\\nMerci."}
   :de {:contactIntro "Kontakt: "      :redo "Neu anfangen"
        :toggleSummaryStyle "Den Stil der Zusammenfassung ändern"
        :mailToMessage "Per E-Mail senden" :mailSubject "Ergebnisse"
        :mailBody "Hallo!\\n%s\\nDanke."}
   :sv {:contactIntro "Kontakt: "      :redo "Redo"
        :toggleSummaryStyle "Växla sammanfattningsstil"
        :mailToMessage "Skicka med e-post" :mailSubject "Resultat"
        :mailBody "Hej,\\n%s\\nTack."}})

(defn- kebab->camel
  "Convert a kebab-case keyword to camelCase: :mail-subject → :mailSubject."
  [k]
  (let [parts (str/split (name k) #"-")]
    (keyword (apply str (first parts)
                    (map str/capitalize (rest parts))))))

(defn- convert-ui-strings
  "Convert ui-strings from config (kebab-case keys) to camelCase."
  [m]
  (when m (into {} (map (fn [[k v]] [(kebab->camel k) v]) m))))

;; ---------------------------------------------------------------------------
;; Status → CSS class mapping
;; ---------------------------------------------------------------------------

(def ^:private status->css
  {"positive" "status-positive"
   "neutral"  "status-neutral"
   "caution"  "status-caution"
   "negative" "status-negative"})

;; ---------------------------------------------------------------------------
;; Tree normalization & pre-processing
;;
;; All data transformations happen here in Clojure. The JS runtime only
;; handles DOM rendering, navigation, and score accumulation.
;; ---------------------------------------------------------------------------

(defn- normalize-choice
  "Expand compact choice syntax:
     {:Oui \"next\" :status \"positive\"} →
     {:answer \"Oui\" :goto \"next\" :status \"positive\"}"
  [choice]
  (if (:answer choice)
    choice
    (let [known #{:status :summary :score :explain}
          [ak gv] (first (filter (fn [[k v]]
                                   (and (string? v) (not (known k))))
                                 choice))]
      (if ak
        (-> (dissoc choice ak)
            (assoc :answer (name ak) :goto gv))
        choice))))

(defn- normalize-choices
  "Normalize choices: list of maps or compact map of {answer: goto}."
  [choices]
  (cond
    (vector? choices) (mapv normalize-choice choices)
    (map? choices)    (mapv (fn [[k v]]
                              (if (string? v)
                                {:answer (name k) :goto v}
                                (normalize-choice (assoc v :answer (name k)))))
                            choices)
    :else             choices))

(defn- preprocess-choice
  "Convert markdown to HTML and map status to CSS class in a choice."
  [choice]
  (cond-> choice
    (:answer choice)  (update :answer md->html)
    (:explain choice) (update :explain md->html)
    (:status choice)  (update :status #(get status->css % %))
    ;; Pre-strip markdown from summary for email use (keep raw for display)
    (:summary choice) (assoc :summaryText (let [s (:summary choice)]
                                            (if (string? s) (md-strip s) s)))))

(defn- parse-progress
  "Parse progress string \"[current max]\" into {:value N :max M}."
  [s]
  (when s
    (when-let [[_ cur mx] (re-matches #"\[(\d+)\s+(\d+)\]" (str s))]
      {:value (parse-long cur) :max (parse-long mx)})))

(defn- preprocess-node
  "Pre-process a tree node: normalize choices, convert markdown to HTML,
   parse progress, map status values to CSS classes."
  [node]
  (cond-> node
    (:text node)     (update :text md->html)
    (:help node)     (update :help md->html)
    (:progress node) (assoc :progress (parse-progress (:progress node)))
    (:choices node)  (update :choices #(->> (normalize-choices %)
                                            (mapv preprocess-choice)))))

(defn- preprocess-tree
  "Normalize and pre-process all tree nodes."
  [tree]
  (let [nodes (mapv preprocess-node tree)]
    {:nodes nodes
     :home  (some #(when (:home-page %) (:node %)) nodes)
     :start (some #(when (:start-page %) (:node %)) nodes)}))

;; ---------------------------------------------------------------------------
;; CSS framework abstraction
;;
;; Each framework provides: CDN URL, CSS class names for grid/cell/button,
;; and trees-specific CSS overrides.  The JS runtime reads class names
;; from CFG so it stays framework-agnostic.
;; ---------------------------------------------------------------------------

(def ^:private base-css
  "/* Status colors */
:root{
  --status-positive:#18753c;--status-positive-bg:color-mix(in srgb,#18753c 10%,transparent);
  --status-neutral:#0063cb;--status-neutral-bg:color-mix(in srgb,#0063cb 10%,transparent);
  --status-caution:#b34000;--status-caution-bg:color-mix(in srgb,#b34000 10%,transparent);
  --status-negative:#ce0500;--status-negative-bg:color-mix(in srgb,#ce0500 10%,transparent)}
/* Layout */
header{text-align:center;padding:2rem 1rem;background:#fafafa;border-bottom:2px solid #e0e0e0}
header h1{font-size:1.8rem;margin:0 0 .3rem}
header h2{font-size:1.1rem;font-weight:400;color:#666;margin:0}
main{max-width:720px;margin:0 auto;padding:1.5rem 1rem 2rem}
footer{text-align:center;padding:1.5rem 1rem;font-size:.875em;color:#999;border-top:1px solid #e0e0e0;margin-top:2rem}
/* Status */
.status-positive{border-color:var(--status-positive)!important;color:var(--status-positive)}
.status-positive:hover{background:var(--status-positive-bg)!important}
.status-neutral{border-color:var(--status-neutral)!important;color:var(--status-neutral)}
.status-neutral:hover{background:var(--status-neutral-bg)!important}
.status-caution{border-color:var(--status-caution)!important;color:var(--status-caution)}
.status-caution:hover{background:var(--status-caution-bg)!important}
.status-negative{border-color:var(--status-negative)!important;color:var(--status-negative)}
.status-negative:hover{background:var(--status-negative-bg)!important}
/* Actions */
nav.actions{display:flex;gap:1rem;margin-bottom:1.5rem}
nav.actions a{cursor:pointer;font-size:1.3em;text-decoration:none;padding:.25rem}
/* Summary */
.summary{margin-top:1rem}
.summary article{margin:0 0 .6rem;padding:.75rem 1rem;background:#f8f8f8;
  border-radius:.25rem;border-left:3px solid #ddd}
/* Score */
.score-result{margin:1.25rem 0;padding:1rem 1.2rem;border-radius:.25rem;font-weight:600}
.score-result.status-positive{background:var(--status-positive-bg);color:var(--status-positive);border:1px solid var(--status-positive)}
.score-result.status-neutral{background:var(--status-neutral-bg);color:var(--status-neutral);border:1px solid var(--status-neutral)}
.score-result.status-caution{background:var(--status-caution-bg);color:var(--status-caution);border:1px solid var(--status-caution)}
.score-result.status-negative{background:var(--status-negative-bg);color:var(--status-negative);border:1px solid var(--status-negative)}
.trees{margin-top:1.5rem}")

(def ^:private frameworks
  {:pure
   {:cdn     "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css"
    :classes {:grid "pure-g" :cell "pure-u-1" :button "pure-button"
              :progress "" :heading ""}
    :css
    (str base-css "
/* Pure base */
body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif;
  color:#333;line-height:1.6}
a{color:#0063cb}
h3{font-size:1.25rem;margin:0 0 1rem}
/* Progress */
progress{width:100%;height:8px;margin-bottom:1.5rem;-webkit-appearance:none;appearance:none;border:none;
  border-radius:4px;overflow:hidden}
progress::-webkit-progress-bar{background:#e0e0e0;border-radius:4px}
progress::-webkit-progress-value{background:#0078e7;border-radius:4px}
progress::-moz-progress-bar{background:#0078e7;border-radius:4px}
/* Grid */
.trees .pure-g{gap:.75rem}
.trees .pure-g .pure-u-1{margin:0;padding:0}
@media(min-width:568px){.trees .pure-g .pure-u-1{width:auto;flex:1 1 200px}}
.trees button.pure-button{width:100%;text-align:left;padding:.85rem 1.1rem;
  border:2px solid #e0e0e0;border-radius:.25rem;background:#fff;color:#333;
  font-size:1rem;line-height:1.4;transition:border-color .15s,background .15s}
.trees button.pure-button:hover{border-color:#0078e7;background:#f0f6ff}
.trees button.pure-button div{display:flex;flex-direction:column}
.trees button.pure-button small{font-size:.82em;color:#888;margin-top:.4rem}
/* Help */
aside.help{margin:0 0 1.5rem;padding:1rem 1.2rem;background:#f5f7ff;
  border-radius:.25rem;border-left:4px solid #0078e7;font-size:.95em}")}

   :pico
   {:cdn     "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"
    :classes {:grid "trees-grid" :cell "trees-cell" :button "outline"
              :progress "" :heading ""}
    :css
    "/* Status colors */
:root{
  --status-positive:#18753c;--status-positive-bg:color-mix(in srgb,#18753c 10%,transparent);
  --status-neutral:#0063cb;--status-neutral-bg:color-mix(in srgb,#0063cb 10%,transparent);
  --status-caution:#b34000;--status-caution-bg:color-mix(in srgb,#b34000 10%,transparent);
  --status-negative:#ce0500;--status-negative-bg:color-mix(in srgb,#ce0500 10%,transparent)}
/* Layout */
header{text-align:center}
header h1{font-size:1.8rem;margin:0 0 .3rem}
header h2{font-size:1.1rem;font-weight:400;margin:0}
main{max-width:720px;margin:0 auto}
/* Status */
.status-positive{border-color:var(--status-positive)!important;color:var(--status-positive)}
.status-positive:hover{background:var(--status-positive-bg)!important}
.status-neutral{border-color:var(--status-neutral)!important;color:var(--status-neutral)}
.status-neutral:hover{background:var(--status-neutral-bg)!important}
.status-caution{border-color:var(--status-caution)!important;color:var(--status-caution)}
.status-caution:hover{background:var(--status-caution-bg)!important}
.status-negative{border-color:var(--status-negative)!important;color:var(--status-negative)}
.status-negative:hover{background:var(--status-negative-bg)!important}
/* Actions */
nav.actions{display:flex;gap:1rem;margin-bottom:1.5rem}
nav.actions a{cursor:pointer;font-size:1.3em;text-decoration:none;padding:.25rem}
/* Summary */
.summary{margin-top:1rem}
.summary article{margin:0 0 .6rem;padding:.75rem 1rem;
  background:var(--pico-card-background-color);
  border-radius:.25rem;border-left:3px solid var(--pico-muted-border-color)}
/* Score */
.score-result{margin:1.25rem 0;padding:1rem 1.2rem;border-radius:.25rem;font-weight:600}
.score-result.status-positive{background:var(--status-positive-bg);color:var(--status-positive);border:1px solid var(--status-positive)}
.score-result.status-neutral{background:var(--status-neutral-bg);color:var(--status-neutral);border:1px solid var(--status-neutral)}
.score-result.status-caution{background:var(--status-caution-bg);color:var(--status-caution);border:1px solid var(--status-caution)}
.score-result.status-negative{background:var(--status-negative-bg);color:var(--status-negative);border:1px solid var(--status-negative)}
/* Grid */
.trees{margin-top:1.5rem}
.trees-grid{display:flex;flex-wrap:wrap;gap:.75rem}
.trees-cell{flex:1 1 200px;min-width:0}
.trees button{width:100%;text-align:left;padding:.85rem 1.1rem;
  border:2px solid var(--pico-muted-border-color);border-radius:.25rem;
  font-size:1rem;line-height:1.4;transition:border-color .15s,background .15s}
.trees button:hover{border-color:var(--pico-primary);background:var(--pico-primary-focus)}
.trees button div{display:flex;flex-direction:column}
.trees button small{font-size:.82em;opacity:.6;margin-top:.4rem}
/* Help */
aside.help{margin:0 0 1.5rem;padding:1rem 1.2rem;background:var(--pico-card-background-color);
  border-radius:.25rem;border-left:4px solid var(--pico-primary);font-size:.95em}"}

   :bulma
   {:cdn     "https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css"
    :classes {:grid "columns is-multiline" :cell "column is-narrow" :button "button"
              :progress "progress is-small is-primary" :heading "title is-4"}
    :css
    (str base-css "
/* Progress */
progress.progress{margin-bottom:1.5rem}
/* Grid */
.trees .columns{gap:.5rem}
.trees .column{padding:.25rem}
.trees button.button{width:100%;height:auto;white-space:normal;text-align:left;
  padding:.85rem 1.1rem;border:2px solid #e0e0e0;border-radius:.25rem;
  font-size:1rem;line-height:1.4;transition:border-color .15s,background .15s}
.trees button.button:hover{border-color:#485fc7;background:#f0f3ff}
.trees button.button div{display:flex;flex-direction:column}
.trees button.button small{font-size:.82em;color:#888;margin-top:.4rem}
/* Bulma needs !important on color to override its defaults */
.status-positive,.status-neutral,.status-caution,.status-negative{color:inherit!important}
.status-positive{color:var(--status-positive)!important}
.status-neutral{color:var(--status-neutral)!important}
.status-caution{color:var(--status-caution)!important}
.status-negative{color:var(--status-negative)!important}
/* Help */
aside.help{margin:0 0 1.5rem;padding:1rem 1.2rem;background:#f5f7ff;
  border-radius:.25rem;border-left:4px solid #485fc7;font-size:.95em}")}})

(defn- get-framework
  "Look up framework by name. Defaults to :pico."
  [fw-name]
  (get frameworks (keyword (or fw-name "pico")) (:pico frameworks)))

;; ---------------------------------------------------------------------------
;; JavaScript runtime (minimal: DOM rendering, navigation, score tracking)
;;
;; All data transformations (markdown, status mapping, progress parsing)
;; are done in Clojure above. The JS only does what requires browser
;; interactivity: DOM manipulation, hash-based navigation, and score
;; accumulation across user selections.
;; ---------------------------------------------------------------------------

(def ^:private js-runtime "
'use strict';
var HOME=CFG.home,START=CFG.start;
var hist=[{score:JSON.parse(JSON.stringify(SV))}],showA=true;
function dc(o){return JSON.parse(JSON.stringify(o))}
function gn(id){return TREE.find(function(n){return n.node===id})}
function sh(s){return s?s.replace(/<[^>]+>/g,''):''}
function ms(p,c){var r=dc(p);for(var k in c){if(r[k]){var a=r[k].value,b=c[k].value;
  r[k].value=(typeof a==='number'&&typeof b==='number')?a+b:b}else r[k]=dc(c[k])}return r}
function fs(s){var r={};for(var k in s)r[k]=s[k].value;return r}
function csr(scores){if(!CSO)return null;var m=[];
  for(var cn in CSO){var c=CSO[cn],ok=true;
    for(var ck in c){if(ck==='status'||ck==='message'||ck==='priority'||ck==='node')continue;
      var cd=c[ck];for(var sk in cd){var th=cd[sk],ac=scores[sk]||0;
        if(th===0?ac!==0:ac<th)ok=false}}
    if(ok)m.push({msg:c.message,status:c.status,pri:c.priority||0})}
  if(!m.length)return null;m.sort(function(a,b){return(a.pri||0)-(b.pri||0)});return m[0]}
function fmt(o,scores){for(var k in scores){var v=scores[k],sv=SV[k];
  if(sv&&sv['as-percent']&&sv.max)v=Math.round((v*100)/sv.max);
  o=o.replace('%'+k+'%',v)}return o}
function el(t,c,h){var e=document.createElement(t);if(c)e.className=c;if(h)e.innerHTML=h;return e}

function render(){
  var hash=location.hash.replace('#','')||HOME||'0',node=gn(hash);
  if(!node)node=gn(HOME||'0');
  var app=document.getElementById('app');app.innerHTML='';
  if(CFG.hasHeader)app.appendChild(el('header','',CFG.headerHtml));
  var main=el('main','','');
  if(node.progress){var p=el('progress',CFG.cls.progress,'');p.value=node.progress.value;p.max=node.progress.max;main.appendChild(p)}
  main.appendChild(el('h3',CFG.cls.heading,node.text));
  if(node.done){
    var lh=hist[hist.length-1];
    var nav=el('nav','actions','');
    var tl=el('a','','\\ud83d\\udd17');tl.title=I18N.toggleSummaryStyle;tl.href='javascript:void(0)';
    tl.onclick=function(){showA=!showA;render()};nav.appendChild(tl);
    var rl=el('a','','\\ud83d\\udd03');rl.title=I18N.redo;rl.href='#'+(START||HOME||'0');
    rl.onclick=function(){hist=[{score:dc(SV)}]};nav.appendChild(rl);
    if(CFG.mailTo){var ml=el('a','','\\ud83d\\udce9');ml.title=I18N.mailToMessage;
      var its=showA?(lh.answers||[]).filter(Boolean):(lh.questions||[]).filter(Boolean);
      var bp=its.map(function(i){return Array.isArray(i)?i.filter(Boolean).map(sh).join(' \\u2192 '):sh(String(i))}).reverse().join('\\n');
      ml.href='mailto:'+CFG.mailTo+'?subject='+encodeURIComponent(I18N.mailSubject)+'&body='+encodeURIComponent(bp);
      nav.appendChild(ml)}
    main.appendChild(nav);
    if(node.help)main.appendChild(el('aside','help',node.help));
    var sec=el('section','summary','');
    if(CFG.displayScore){var flat=fs(lh.score);
      if(CSO){var res=csr(flat);
        if(res&&res.msg)sec.appendChild(el('div','score-result '+(res.status||''),fmt(res.msg,flat)))}
      if(CFG.displayScoreDetails){var dd=el('div','score-details','');
        for(var sk in lh.score)dd.appendChild(el('span','',(lh.score[sk].display||sk)+': '+lh.score[sk].value+'  '));
        sec.appendChild(dd)}
      if(typeof CFG.displayUnconditionally==='string'&&CFG.displayUnconditionally)
        sec.appendChild(el('aside','',CFG.displayUnconditionally))}
    if(CFG.displaySummary){
      var its=showA?(lh.answers||[]).filter(Boolean).reverse():(lh.questions||[]).filter(Boolean).reverse();
      its.forEach(function(o){var art=el('article','','');
        if(typeof o==='string')art.innerHTML='<div>'+o+'</div>';
        else if(Array.isArray(o)&&o.length>0){var h='';
          if(o.length>=2&&o[0])h+=o[0];
          if(o.length>=2&&o[o.length-1])h+=' <strong>'+o[o.length-1]+'</strong>';
          else if(o.length===1&&o[0])h+=o[0];
          art.innerHTML='<div>'+h+'</div>'}
        sec.appendChild(art)})}
    main.appendChild(sec)
  }else{
    if(node.help)main.appendChild(el('aside','help',node.help));
    if(node.choices){var cd=el('div','trees',''),grid=el('div',CFG.cls.grid,'');
      node.choices.forEach(function(ch){var cell=el('div',CFG.cls.cell,'');
        var btn=document.createElement('button');btn.className=CFG.cls.button+' '+(ch.status||'');
        var bh=ch.answer;if(ch.explain)bh+='<br><small>'+ch.explain+'</small>';
        btn.innerHTML='<div>'+bh+'</div>';
        btn.onclick=function(){
          if(Array.isArray(ch.summary)&&ch.summary.length>1)alert(ch.summary[ch.summary.length-1]);
          var ph=hist[hist.length-1],cs=ms(ph.score,ch.score||{});
          var gt=ch.goto;if(typeof gt==='object'&&gt!==null)gt=gt['default']||Object.values(gt)[0]||'end';
          hist.push({score:cs,
            questions:(ph.questions||[]).concat(node['no-summary']?[null]:[[node.text,ch.answer]]),
            answers:(ph.answers||[]).concat([ch.summaryText||ch.summary||null])});
          location.hash=gt};
        cell.appendChild(btn);grid.appendChild(cell)});
      cd.appendChild(grid);main.appendChild(cd)}}
  app.appendChild(main);
  if(CFG.hasFooter)app.appendChild(el('footer','',CFG.footerHtml))}
addEventListener('hashchange',function(){
  var h=location.hash.replace('#','');
  if(h===(START||HOME||'0'))hist=[{score:dc(SV)}];render()});
render();")

;; ---------------------------------------------------------------------------
;; CSS theme resolution
;; ---------------------------------------------------------------------------

(defn- http-url? [s]
  (or (str/starts-with? s "http://")
      (str/starts-with? s "https://")))

(defn resolve-css-theme
  "Resolve a theme value to {:link url} or {:inline content}.
  Priority: https:// URL, file:/// URL, local .css path, pico-themes name."
  [css-theme]
  (when css-theme
    (cond
      (http-url? css-theme)
      {:link css-theme}
      (str/starts-with? css-theme "file:///")
      (let [path (subs css-theme 7)]
        (if (.exists (java.io.File. path))
          {:inline (slurp path)}
          (do (println "Warning: theme file not found:" path) nil)))
      (and (str/ends-with? css-theme ".css")
           (.exists (java.io.File. css-theme)))
      {:inline (slurp css-theme)}
      (not (str/includes? css-theme " "))
      {:link (str "https://cdn.jsdelivr.net/gh/bzg/pico-themes@latest/" css-theme ".css")}
      :else
      (do (println "Warning: unrecognized CSS theme value:" css-theme) nil))))

;; ---------------------------------------------------------------------------
;; HTML generation
;; ---------------------------------------------------------------------------

(defn generate-html [config]
  (let [locale  (keyword (or (:locale config) "en"))
        fw      (get-framework (:framework config))
        i18n    (merge (get i18n-strings :en)
                       (get i18n-strings locale)
                       (convert-ui-strings (:ui-strings config)))
        {:keys [nodes home start]} (preprocess-tree (:tree config))
        header  (:header config)
        footer  (:footer config)
        resolved  (resolve-css-theme (or (:css-theme config)
                                        (:theme-url config)
                                        (:theme config)))
        theme-url (:link resolved)
        theme-css (:inline resolved)
        header-html (str "<div><h1>" (escape-html (or (:title header) ""))
                         "</h1><h2>" (escape-html (or (:subtitle header) "")) "</h2></div>")
        footer-html (str "<div>" (escape-html (or (:text footer) ""))
                         (when-let [c (:contact footer)]
                           (str "<p>" (escape-html (:contactIntro i18n))
                                "<a href=\"mailto:" (escape-html c) "\">"
                                (escape-html c) "</a></p>"))
                         "</div>")
        cfg-json  (clj->json {:displaySummary         (boolean (:display-summary config))
                               :displayScore           (boolean (:display-score config))
                               :displayScoreDetails    (boolean (:display-score-details config))
                               :displayUnconditionally (or (:display-unconditionally config) false)
                               :mailTo                 (or (:mail-to config) "")
                               :home                   home
                               :start                  start
                               :cls                    (:classes fw)
                               :hasHeader              (boolean (not-empty header))
                               :hasFooter              (boolean (not-empty footer))
                               :headerHtml             header-html
                               :footerHtml             footer-html})]
    (->> ["<!DOCTYPE html>"
          (str "<html lang=\"" (name locale) "\">")
          "<head>"
          "<meta charset=\"utf-8\"/>"
          "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>"
          (str "<title>" (escape-html (or (:title header) "Trees")) "</title>")
          (str "<link rel=\"stylesheet\" href=\"" (:cdn fw) "\"/>")
          (str "<style>" (:css fw) "</style>")
          (when theme-url (str "<link rel=\"stylesheet\" href=\"" theme-url "\"/>"))
          (when theme-css (str "<style>" theme-css "</style>"))
          "</head>"
          "<body>"
          "<div id=\"app\"></div>"
          "<script>"
          (str "var TREE=" (clj->json nodes) ";")
          (str "var SV=" (clj->json (or (:score-variables config) {})) ";")
          (str "var CSO=" (clj->json (:conditional-score-output config)) ";")
          (str "var CFG=" cfg-json ";")
          (str "var I18N=" (clj->json i18n) ";")
          js-runtime
          "</script>"
          "</body>"
          "</html>"]
         (remove nil?)
         (str/join "\n"))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [config-file (or (first *command-line-args*) "config.yml")
      output-file (or (second *command-line-args*) "index.html")]
  (when-not (.exists (java.io.File. config-file))
    (println (str "Error: " config-file " not found."))
    (System/exit 1))
  (let [config (parse-yaml (slurp config-file))
        html   (generate-html config)]
    (spit output-file html)
    (println (str "Generated " output-file " from " config-file
                  " (" (count (:tree config)) " nodes)"))))
