(ns datomic.codeq.git
  (:require [clojure.string  :as string]
            [clojure.java.io :as io]))

(defn ^java.io.Reader exec-stream
  [^String cmd]
  (-> (Runtime/getRuntime)
      (.exec cmd)
      .getInputStream
      io/reader))

(defn git-cmd
  [dir cmd & args]
  (apply str
         "git --git-dir=" dir
;         "--work-tree=" dir
         " " cmd (interpose " " args)))

(defn git-stream
  [dir cmd & args]
  (exec-stream (git-cmd dir cmd args)))

;;example commit - git cat-file -p
;;tree d81cd432f2050c84a3d742caa35ccb8298d51e9d
;;author Rich Hickey <richhickey@gmail.com> 1348842448 -0400
;;committer Rich Hickey <richhickey@gmail.com> 1348842448 -0400

;; or

;;tree ba63180c1d120b469b275aef5da479ab6c3e2afd
;;parent c3bd979cfe65da35253b25cb62aad4271430405c
;;maybe more parents
;;author Rich Hickey <richhickey@gmail.com> 1348869325 -0400
;;committer Rich Hickey <richhickey@gmail.com> 1348869325 -0400
;;then blank line
;;then commit message


;;example tree
;;100644 blob ee508f768d92ba23e66c4badedd46aa216963ee1	.gitignore
;;100644 blob b60ea231eb47eb98395237df17550dee9b38fb72	README.md
;;040000 tree bcfca612efa4ff65b3eb07f6889ebf73afb0e288	doc
;;100644 blob 813c07d8cd27226ddd146ddd1d27fdbde10071eb	epl-v10.html
;;100644 blob f8b5a769bcc74ee35b9a8becbbe49d4904ab8abe	project.clj
;;040000 tree 6b880666740300ac57361d5aee1a90488ba1305c	src
;;040000 tree 407924e4812c72c880b011b5a1e0b9cb4eb68cfa	test

;; example git remote origin
;;RichMacPro:codeq rich$ git remote show -n origin
;;* remote origin
;;  Fetch URL: https://github.com/Datomic/codeq.git
;;  Push  URL: https://github.com/Datomic/codeq.git
;;  HEAD branch: (not queried)

(defn dir
  "Returns [[sha :type filename] ...]"
  [dir tree]
  (with-open [s (git-stream dir (str "cat-file -p " tree))]
    (let [es (line-seq s)]
      (mapv #(let [ss (string/split ^String % #"\s")]
               [(nth ss 2)
                (keyword (nth ss 1))
               (subs % (inc (.indexOf ^String % "\t")) (count %))])
            es))))

(defn commits
  "Returns log as [[sha msg] ...], in commit order. commit-name may be
  nil or any acceptable commit name arg for git log"
  ([dir]
     (commits dir nil))
  ([dir commit-name]
     (with-open [s (git-stream dir (str "log --pretty=oneline --date-order --reverse " commit-name))]
       (mapv
        #(vector (subs % 0 40)
                 (subs % 41 (count %)))
        (line-seq s)))))

(defn commit
  [dir sha]
  (let [trim-email (fn [s] (subs s 1 (dec (count s))))
        dt (fn [ds] (java.util.Date. (* 1000 (Integer/parseInt ds))))
        [tree parents author committer msg]
        (with-open [s (git-stream dir (str "cat-file -p " sha))]
          (let [lines (line-seq s)
                slines (mapv #(string/split % #"\s") lines)
                tree (-> slines (nth 0) (nth 1))
                [plines xs] (split-with #(= (nth % 0) "parent") (rest slines))]
            [tree
             (seq (map second plines))
             (vec (reverse (first xs)))
             (vec (reverse (second xs)))
             (->> lines
                  (drop-while #(not= % ""))
                  rest
                  (interpose "\n")
                  (apply str))]))]
    {:sha sha
     :msg msg
     :tree tree
     :parents parents
     :author (trim-email (author 2))
     :authored (dt (author 1))
     :committer (trim-email (committer 2))
     :committed (dt (committer 1))}))

(defn blob-text
  [dir sha]
  (with-open [s (git-stream dir (str "cat-file -p " sha))]
    (slurp s)))

(defn repo-uri
  [dir]
  (with-open [s (git-stream dir "remote show -n origin")]
    (let [es (line-seq s)
          ^String line (second es)
          uri (subs line (inc (.lastIndexOf line " ")))
          noff (.lastIndexOf uri "/")
          noff (if (not (pos? noff)) (.lastIndexOf uri ":") noff)
          name (subs uri (inc noff))
          _ (assert (pos? (count name)) "Can't find remote origin")
          name (if (.endsWith name ".git") (subs name 0 (.indexOf name ".")) name)]
      [uri name])))
