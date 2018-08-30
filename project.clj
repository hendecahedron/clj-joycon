(defproject clj-joycon "0.1.0-SNAPSHOT"
  :description "clj-joycon"
  :url         "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.java.dev.jna/jna "4.5.2"]]
  :java-source-paths ["src/java"]
  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["lib/purejavahidapi.jar"])
