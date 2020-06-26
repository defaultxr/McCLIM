(in-package #:asdf-user)

(defsystem "clim-core"
  :depends-on ("clim-basic" #+sbcl (:require "sb-introspect"))
  :components
  ((:file "defresource")
   (:file "theming")
   (:module "presentations"
    :serial t
    :components ((:file "presentation-types")
                 (:file "presentation-functions")
                 (:file "presentation-type-functions")
                 (:file "typed-output")
                 (:file "typed-input")
                 (:file "translators")
                 (:file "drag-and-drop")))
   (:file "bordered-output" :depends-on ("presentations" "theming"))
   (:file "table-formatting" :depends-on ("presentations"))
   (:file "input-editing" :depends-on ("presentations" "bordered-output" "table-formatting"))
   (:file "presentation-defs" :depends-on ("input-editing" "presentations"))
   (:file "graph-formatting")
   (:file "frames" :depends-on ("commands" "presentations" "presentation-defs" "incremental-redisplay"))
   (:file "dialog-views" :depends-on ("presentations" "incremental-redisplay" "bordered-output" "presentation-defs" "gadgets" "dialog"))
   (:module "gadgets"
    :depends-on ("commands" "input-editing" "frames" "incremental-redisplay" "panes" "presentations" "theming")
    :serial t
    :components ((:file "base")
                 (:file "abstract")
                 (:file "mixins")
                 (:file "drawing-utilities")
                 (:file "concrete")
                 (:file "menu")))
   (:file "describe" :depends-on ("presentations" "presentation-defs" "table-formatting"))
   (:file "commands" :depends-on ("input-editing" "presentations" "presentation-defs"))
   (:file "incremental-redisplay" :depends-on ("presentation-defs"))
   (:file "menu-choose" :depends-on ("commands" "table-formatting" "presentation-defs" "panes" "frames" "presentations"))
   (:file "panes" :depends-on ("incremental-redisplay" "presentations" "presentation-defs" "input-editing" "frames" "theming"))
   (:file "dialog" :depends-on ("panes" "frames" "incremental-redisplay" "table-formatting" "presentations" "bordered-output" "presentation-defs" "input-editing" "commands" "gadgets"))
   (:file "builtin-commands" :depends-on ("table-formatting" "commands" "presentations" "dialog" "presentation-defs" "input-editing"))))
