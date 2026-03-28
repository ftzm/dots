((ace-window :source "elpaca-menu-lock-file" :recipe
	     (:package "ace-window" :repo "abo-abo/ace-window"
		       :fetcher github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
		    (:package "aggressive-indent" :repo
			      "Malabarba/aggressive-indent-mode"
			      :fetcher github :files
			      ("*.el" "*.el.in" "dir" "*.info"
			       "*.texi" "*.texinfo" "doc/dir"
			       "doc/*.info" "doc/*.texi"
			       "doc/*.texinfo" "lisp/*.el" "docs/dir"
			       "docs/*.info" "docs/*.texi"
			       "docs/*.texinfo"
			       (:exclude ".dir-locals.el" "test.el"
					 "tests.el" "*-test.el"
					 "*-tests.el" "LICENSE"
					 "README*" "*-pkg.el"))
			      :source "MELPA" :protocol https :inherit
			      t :depth treeless :ref
			      "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (annalist :source "elpaca-menu-lock-file" :recipe
	   (:package "annalist" :fetcher github :repo
		     "noctuid/annalist.el" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (autothemer :source "elpaca-menu-lock-file" :recipe
	     (:package "autothemer" :fetcher github :repo
		       "jasonm23/autothemer" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "e62bf83414abd8b1cefafb7480612faa30ed7878"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		 "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el"
			   "*-test.el" "*-tests.el" "LICENSE"
			   "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth
		treeless :ref
		"933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (bui :source "elpaca-menu-lock-file" :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		 "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el"
			   "*-test.el" "*-tests.el" "LICENSE"
			   "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth
		treeless :ref
		"f3a137628e112a91910fd33c0cff0948fa58d470"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el"
			    "*-test.el" "*-tests.el" "LICENSE"
			    "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth
		 treeless :ref
		 "e608ccb4c19caabae3fe37d71e41891feec23601"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github
		 :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el"
			    "*-test.el" "*-tests.el" "LICENSE"
			    "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth
		 treeless :ref
		 "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (claude-code-ide :source "elpaca-menu-lock-file" :recipe
		  (:source "Init file" :protocol https :inherit t
			   :depth treeless :host github :repo
			   "manzaltu/claude-code-ide.el" :package
			   "claude-code-ide" :ref
			   "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364"))
 (claudemacs :source "elpaca-menu-lock-file" :recipe
	     (:source "Init file" :protocol https :inherit t :depth
		      treeless :host github :repo "cpoile/claudemacs"
		      :package "claudemacs" :ref
		      "639b5a7986e10b8812bf987d39a21fe603879bea"))
 (cmake-mode :source "elpaca-menu-lock-file" :recipe
	     (:package "cmake-mode" :fetcher git :url
		       "https://gitlab.kitware.com/cmake/cmake.git"
		       :files ("Auxiliary/*.el") :source "MELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "c1c6b13eb343fe1cb1d8bbfc9ebad284fd902b34"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
	     :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
	      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
	      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
	      "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el"
			"*-test.el" "*-tests.el" "LICENSE" "README*"
			"*-pkg.el"))
	     :source "MELPA" :protocol https :inherit t :depth
	     treeless :host github :ref
	     "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
	  (:package "consult" :repo "minad/consult" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "f8c2ef57e83af3d45e345e5c14089f2f9973682b"))
 (consult-eglot :source "elpaca-menu-lock-file" :recipe
		(:package "consult-eglot" :fetcher github :repo
			  "mohkale/consult-eglot" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t
			  :depth treeless :ref
			  "d8b444aac39edfc6473ffbd228df3e9119451b51"))
 (corfu :source "elpaca-menu-lock-file" :recipe
	(:package "corfu" :repo "minad/corfu" :files
		  (:defaults "extensions/corfu-*.el") :fetcher github
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :ref
		  "d2a995c5c732d0fc439efe09440870a9de779a74"))
 (dap-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher
		     github :files (:defaults "icons") :source "MELPA"
		     :protocol https :inherit t :depth treeless :ref
		     "b77d9bdb15d89e354b8a20906bebe7789e19fc9b"))
 (dape :source "elpaca-menu-lock-file" :recipe
       (:package "dape" :repo
		 ("https://github.com/svaante/dape" . "dape") :files
		 ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
		 https :inherit t :depth treeless :ref
		 "1e86212784198f6d3185d712dd6b724601052118"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
		 ("dash.el" "dash.texi") :source "MELPA" :protocol
		 https :inherit t :depth treeless :ref
		 "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
	  (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl"
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "bb9af85441b0cbb3281268d30256d50f0595ebfe"))
 (direnv :source "elpaca-menu-lock-file" :recipe
	 (:package "direnv" :fetcher github :repo
		   "wbolster/emacs-direnv" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth
		   treeless :ref
		   "c0bf3b81c7a97e2a0d06d05495e86848254fcc1f"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
		(:package "doom-modeline" :repo
			  "seagle0128/doom-modeline" :fetcher github
			  :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t
			  :depth treeless :ref
			  "e3ad151fe958865cd4f25ce91feb88a9810d896d"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
	      (:package "doom-themes" :fetcher github :repo
			"doomemacs/themes" :files
			(:defaults "themes/*.el" "themes/*/*.el"
				   "extensions/*.el")
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"53645a905dfb3055db52f5d418d5ef612027e062"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo "akib/emacs-eat" :files
		("*.el" ("term" "term/*.el") "*.texi" "*.ti"
		 ("terminfo/e" "terminfo/e/*")
		 ("terminfo/65" "terminfo/65/*")
		 ("integration" "integration/*")
		 (:exclude ".dir-locals.el" "*-tests.el"))
		:source "NonGNU ELPA" :protocol https :inherit t
		:depth treeless :type git :host codeberg :branch
		"master" :ref
		"c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
	       (:package "editorconfig" :fetcher github :repo
			 "editorconfig/editorconfig-emacs" :old-names
			 (editorconfig-core editorconfig-fnmatch)
			 :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			  "*.texinfo" "doc/dir" "doc/*.info"
			  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			  "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el"
				    "tests.el" "*-test.el"
				    "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t
			 :depth treeless :ref
			 "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"))
 (eglot-booster :source "elpaca-menu-lock-file" :recipe
		(:source "Init file" :protocol https :inherit t :depth
			 treeless :host github :repo
			 "jdtsmith/eglot-booster" :package
			 "eglot-booster" :ref
			 "cab7803c4f0adc7fff9da6680f90110674bb7a22"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
	     (:package "elisp-refs" :repo "Wilfred/elisp-refs"
		       :fetcher github :files
		       (:defaults (:exclude "elisp-refs-bench.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
	    "https://github.com/progfolio/elpaca.git" :ref
	    "66e1303ae5aa76dcf84e5c974e39f093d5e19dbc" :files
	    (:defaults "elpaca-test.el" (:exclude "extensions"))
	    :build (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
		     (:package "elpaca-use-package" :wait t :repo
			       "https://github.com/progfolio/elpaca.git"
			       :files
			       ("extensions/elpaca-use-package.el")
			       :main
			       "extensions/elpaca-use-package.el"
			       :build (:not elpaca--compile-info)
			       :source "Elpaca extensions" :protocol
			       https :inherit t :depth treeless :ref
			       "66e1303ae5aa76dcf84e5c974e39f093d5e19dbc"))
 (embark :source "elpaca-menu-lock-file" :recipe
	 (:package "embark" :repo "oantolin/embark" :fetcher github
		   :files ("embark.el" "embark-org.el" "embark.texi")
		   :source "MELPA" :protocol https :inherit t :depth
		   treeless :ref
		   "e0238889b1c946514fd967d21d70599af9c4e887"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
		 (:package "embark-consult" :repo "oantolin/embark"
			   :fetcher github :files
			   ("embark-consult.el") :source "MELPA"
			   :protocol https :inherit t :depth treeless
			   :ref
			   "e0238889b1c946514fd967d21d70599af9c4e887"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
		 (:defaults "doc/build/texinfo/evil.texi"
			    (:exclude "evil-test-helpers.el"))
		 :source "MELPA" :protocol https :inherit t :depth
		 treeless :ref
		 "729d9a58b387704011a115c9200614e32da3cefc"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
		  (:package "evil-collection" :fetcher github :repo
			    "emacs-evil/evil-collection" :files
			    (:defaults "modes") :source "MELPA"
			    :protocol https :inherit t :depth treeless
			    :ref
			    "8f261eb0c284be23f534dbbef976d71a5ab5245f"))
 (evil-commentary :source "elpaca-menu-lock-file" :recipe
		  (:package "evil-commentary" :repo
			    "linktohack/evil-commentary" :fetcher
			    github :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			     "*.texinfo" "doc/dir" "doc/*.info"
			     "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			     "docs/dir" "docs/*.info" "docs/*.texi"
			     "docs/*.texinfo"
			     (:exclude ".dir-locals.el" "test.el"
				       "tests.el" "*-test.el"
				       "*-tests.el" "LICENSE"
				       "README*" "*-pkg.el"))
			    :source "MELPA" :protocol https :inherit t
			    :depth treeless :ref
			    "c5945f28ce47644c828aac1f5f6ec335478d17fb"))
 (evil-org :source "elpaca-menu-lock-file" :recipe
	   (:package "evil-org" :fetcher github :repo
		     "Somelauw/evil-org-mode" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "b1f309726b1326e1a103742524ec331789f2bf94"))
 (evil-owl :source "elpaca-menu-lock-file" :recipe
	   (:package "evil-owl" :repo "mamapanda/evil-owl" :fetcher
		     github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "a41a6d28e26052b25f3d21da37ccf1d8fde1e6aa"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
	       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
	       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
	       "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el"
			 "*-test.el" "*-tests.el" "LICENSE" "README*"
			 "*-pkg.el"))
	      :source "MELPA" :protocol https :inherit t :depth
	      treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (filladapt :source "elpaca-menu-lock-file" :recipe
	    (:package "filladapt" :repo
		      ("https://github.com/emacsmirror/gnu_elpa"
		       . "filladapt")
		      :branch "externals/filladapt" :files
		      ("*" (:exclude ".git")) :source "GNU ELPA"
		      :protocol https :inherit t :depth treeless :ref
		      "802c1942a7685ebf2af4db021f303d7a767c5915"))
 (flash :source "elpaca-menu-lock-file" :recipe
	(:package "flash" :fetcher github :repo "ftzm/flash" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :host github :branch "overlay-expand" :ref
		  "adb0bf99c27e43c9d09ada565a20ceef96fa2b99"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
	   (:package "flycheck" :repo "flycheck/flycheck" :fetcher
		     github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "0e5eb8300d32fd562724216c19eaf199ee1451ab"))
 (gambit :source "elpaca-menu-lock-file" :recipe
	 (:source "Init file" :protocol https :inherit t :depth
		  treeless :host github :repo "gambit/gambit" :files
		  ("misc/gambit.el") :branch "master" :package
		  "gambit" :ref
		  "e547ae944df670cff4d7055ab8eeb2fbc10a0d00"))
 (general :source "elpaca-menu-lock-file" :recipe
	  (:package "general" :fetcher github :repo
		    "noctuid/general.el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (gerbil-mode :source "elpaca-menu-lock-file" :recipe
	      (:source "Init file" :protocol https :inherit t :depth
		       treeless :host github :repo
		       "mighty-gerbils/gerbil" :files
		       ("etc/gerbil-mode.el") :branch "master"
		       :package "gerbil-mode" :ref
		       "785ded734df99f395f536d0449c998c2914b9e0b"))
 (git-link :source "elpaca-menu-lock-file" :recipe
	   (:package "git-link" :fetcher github :repo "sshaw/git-link"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "d9b375f79e6071a9926bf73bba64111adfc93bf5"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
	   (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher
		     github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gruvbox-theme :source "elpaca-menu-lock-file" :recipe
		(:package "gruvbox-theme" :fetcher github :repo
			  "greduan/emacs-theme-gruvbox" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t
			  :depth treeless :ref
			  "6cbf80b6cde3c2390502dc94a911ab7378495249"))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "haskell-mode" :repo "haskell/haskell-mode"
			 :fetcher github :files
			 (:defaults "NEWS" "logo.svg") :source "MELPA"
			 :protocol https :inherit t :depth treeless
			 :ref
			 "2dd755a5fa11577a9388af88f385d2a8e18f7a8d"))
 (helpful :source "elpaca-menu-lock-file" :recipe
	  (:package "helpful" :repo "Wilfred/helpful" :fetcher github
		    :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		"docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el"
			  "*-test.el" "*-tests.el" "LICENSE" "README*"
			  "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth
	       treeless :ref
	       "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :source "elpaca-menu-lock-file" :recipe
	(:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
		  (:defaults (:exclude "lv.el")) :source "MELPA"
		  :protocol https :inherit t :depth treeless :ref
		  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (jetbrains-darcula-theme :source "elpaca-menu-lock-file" :recipe
			  (:package "jetbrains-darcula-theme" :fetcher
				    github :repo
				    "ianyepan/jetbrains-darcula-emacs-theme"
				    :files
				    ("*.el" "*.el.in" "dir" "*.info"
				     "*.texi" "*.texinfo" "doc/dir"
				     "doc/*.info" "doc/*.texi"
				     "doc/*.texinfo" "lisp/*.el"
				     "docs/dir" "docs/*.info"
				     "docs/*.texi" "docs/*.texinfo"
				     (:exclude ".dir-locals.el"
					       "test.el" "tests.el"
					       "*-test.el"
					       "*-tests.el" "LICENSE"
					       "README*" "*-pkg.el"))
				    :source "MELPA" :protocol https
				    :inherit t :depth treeless :ref
				    "46f153385e50998826ca13e18056c6a972768cfd"))
 (jsonnet-mode :source "elpaca-menu-lock-file" :recipe
	       (:package "jsonnet-mode" :fetcher github :repo
			 "tminor/jsonnet-mode" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			  "*.texinfo" "doc/dir" "doc/*.info"
			  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			  "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el"
				    "tests.el" "*-test.el"
				    "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t
			 :depth treeless :ref
			 "00229c2f04bb4be26686eb325303865dac3cabf8"))
 (libmpdel :source "elpaca-menu-lock-file" :recipe
	   (:package "libmpdel" :fetcher github :repo "mpdel/libmpdel"
		     :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "f2cb01c8d004b5fbfa937579e899035a47d2a5f2"))
 (llama :source "elpaca-menu-lock-file" :recipe
	(:package "llama" :fetcher github :repo "tarsius/llama" :files
		  ("llama.el" ".dir-locals.el") :source "MELPA"
		  :protocol https :inherit t :depth treeless :ref
		  "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (lsp-docker :source "elpaca-menu-lock-file" :recipe
	     (:package "lsp-docker" :repo "emacs-lsp/lsp-docker"
		       :fetcher github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "ff41f4a76b640d39dc238bacba7f654c297827fa"))
 (lsp-metals :source "elpaca-menu-lock-file" :recipe
	     (:package "lsp-metals" :repo "emacs-lsp/lsp-metals"
		       :fetcher github :files (:defaults "icons")
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "e1d9d04f3bab7e6e74916054b36ab1a87e831367"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
		     github :files (:defaults "clients/*.*") :source
		     "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "9a2513cb40cb7daac87efcc63f35c7e066681808"))
 (lsp-treemacs :source "elpaca-menu-lock-file" :recipe
	       (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs"
			 :fetcher github :files (:defaults "icons")
			 :source "MELPA" :protocol https :inherit t
			 :depth treeless :ref
			 "49df7292c521b4bac058985ceeaf006607b497dd"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
	       ("lv.el") :source "MELPA" :protocol https :inherit t
	       :depth treeless :ref
	       "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
	(:package "magit" :fetcher github :repo "magit/magit" :files
		  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
		   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
		   ("git-hooks" "git-hooks/*")
		   (:exclude "lisp/magit-section.el"))
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :ref
		  "59ef32fe7e4fa301ff4df22fe0341d8583695a10"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
		(:package "magit-section" :fetcher github :repo
			  "magit/magit" :files
			  ("lisp/magit-section.el"
			   "docs/magit-section.texi"
			   "magit-section-pkg.el")
			  :source "MELPA" :protocol https :inherit t
			  :depth treeless :ref
			  "59ef32fe7e4fa301ff4df22fe0341d8583695a10"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
	     (:package "marginalia" :repo "minad/marginalia" :fetcher
		       github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "d28a5e5c1a2e5f3e6669b0197f38da84e08f94a0"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
		(:package "markdown-mode" :fetcher github :repo
			  "jrblevin/markdown-mode" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			   "*.texinfo" "doc/dir" "doc/*.info"
			   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   "docs/dir" "docs/*.info" "docs/*.texi"
			   "docs/*.texinfo"
			   (:exclude ".dir-locals.el" "test.el"
				     "tests.el" "*-test.el"
				     "*-tests.el" "LICENSE" "README*"
				     "*-pkg.el"))
			  :source "MELPA" :protocol https :inherit t
			  :depth treeless :ref
			  "182640f79c3ed66f82f0419f130dffc173ee9464"))
 (mpdel :source "elpaca-menu-lock-file" :recipe
	(:package "mpdel" :fetcher github :repo "mpdel/mpdel" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :ref
		  "64cf50aca68064c2857ed0e5a8fddb2075d97090"))
 (navigel :source "elpaca-menu-lock-file" :recipe
	  (:package "navigel" :fetcher github :repo
		    "DamienCassou/navigel" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "539fe2d9542b01824869b98cde000079a1159b9f"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
	     (:package "nerd-icons" :repo
		       "rainstormstudio/nerd-icons.el" :fetcher github
		       :files (:defaults "data") :source "MELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "1db0b0b9203cf293b38ac278273efcfc3581a05f"))
 (nix-ts-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "nix-ts-mode" :fetcher github :repo
			"nix-community/nix-ts-mode" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"319831712125e8c9e4f14b66def518d7a633685c"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
	       (:package "no-littering" :fetcher github :repo
			 "emacscollective/no-littering" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			  "*.texinfo" "doc/dir" "doc/*.info"
			  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			  "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el"
				    "tests.el" "*-test.el"
				    "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t
			 :depth treeless :ref
			 "2b29be2af4249bc4805e3cd9e9b555a62a3dc1a0"))
 (orderless :source "elpaca-menu-lock-file" :recipe
	    (:package "orderless" :repo "oantolin/orderless" :fetcher
		      github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (org-appear :source "elpaca-menu-lock-file" :recipe
	     (:package "org-appear" :fetcher github :repo
		       "awth13/org-appear" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (org-ql :source "elpaca-menu-lock-file" :recipe
	 (:package "org-ql" :fetcher github :repo "alphapapa/org-ql"
		   :files (:defaults (:exclude "helm-org-ql.el"))
		   :source "MELPA" :protocol https :inherit t :depth
		   treeless :ref
		   "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca"))
 (org-super-agenda :source "elpaca-menu-lock-file" :recipe
		   (:package "org-super-agenda" :fetcher github :repo
			     "alphapapa/org-super-agenda" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			      "*.texinfo" "doc/dir" "doc/*.info"
			      "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      "docs/dir" "docs/*.info" "docs/*.texi"
			      "docs/*.texinfo"
			      (:exclude ".dir-locals.el" "test.el"
					"tests.el" "*-test.el"
					"*-tests.el" "LICENSE"
					"README*" "*-pkg.el"))
			     :source "MELPA" :protocol https :inherit
			     t :depth treeless :ref
			     "fb20ad9c8a9705aa05d40751682beae2d094e0fe"))
 (ov :source "elpaca-menu-lock-file" :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		"docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el"
			  "*-test.el" "*-tests.el" "LICENSE" "README*"
			  "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth
	       treeless :ref
	       "e2971ad986b6ac441e9849031d34c56c980cf40b"))
 (paren-face :source "elpaca-menu-lock-file" :recipe
	     (:package "paren-face" :fetcher github :repo
		       "tarsius/paren-face" :files ("paren-face.el")
		       :old-names (parenface) :source "MELPA"
		       :protocol https :inherit t :depth treeless :ref
		       "2c279a236404b2eebacb435aa92d5e9c97939c03"))
 (perspective :source "elpaca-menu-lock-file" :recipe
	      (:package "perspective" :fetcher github :repo
			"nex3/perspective-el" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"64ef5eaaab9e7564e8b9788ce6d0e2359daf5dca"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
	  (:package "pfuture" :repo "Alexander-Miller/pfuture"
		    :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (php-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "php-mode" :repo "emacs-php/php-mode" :fetcher
		     github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "d9858333e42f42c1486a84bc5277e9d8e37e40cc"))
 (popper :source "elpaca-menu-lock-file" :recipe
	 (:package "popper" :fetcher github :repo "karthink/popper"
		   :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		    "doc/*.texinfo" "lisp/*.el" "docs/dir"
		    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		    (:exclude ".dir-locals.el" "test.el" "tests.el"
			      "*-test.el" "*-tests.el" "LICENSE"
			      "README*" "*-pkg.el"))
		   :source "MELPA" :protocol https :inherit t :depth
		   treeless :ref
		   "d83b894ee7a9daf7c8e9b864c23d08f1b23d78f6"))
 (posframe :source "elpaca-menu-lock-file" :recipe
	   (:package "posframe" :fetcher github :repo
		     "tumashu/posframe" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "3a80911b2f45ce6926196930bb7d5cc662c7b3c8"))
 (pretty-hydra :source "elpaca-menu-lock-file" :recipe
	       (:package "pretty-hydra" :repo
			 "jerrypnz/major-mode-hydra.el" :fetcher
			 github :files ("pretty-hydra.el") :source
			 "MELPA" :protocol https :inherit t :depth
			 treeless :ref
			 "2494d71e24b61c1f5ef2dc17885e2f65bf98b3b2"))
 (racket-mode :source "elpaca-menu-lock-file" :recipe
	      (:package "racket-mode" :fetcher github :repo
			"greghendershott/racket-mode" :files
			(:defaults "*.rkt" ("racket" "racket/*")
				   (:exclude "racket/example/*"
					     "racket/test/*"))
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"e5f22ad408740ec517a436ec19b74ce1398e61bc"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
		     (:package "rainbow-delimiters" :fetcher github
			       :repo "Fanael/rainbow-delimiters"
			       :files
			       ("*.el" "*.el.in" "dir" "*.info"
				"*.texi" "*.texinfo" "doc/dir"
				"doc/*.info" "doc/*.texi"
				"doc/*.texinfo" "lisp/*.el" "docs/dir"
				"docs/*.info" "docs/*.texi"
				"docs/*.texinfo"
				(:exclude ".dir-locals.el" "test.el"
					  "tests.el" "*-test.el"
					  "*-tests.el" "LICENSE"
					  "README*" "*-pkg.el"))
			       :source "MELPA" :protocol https
			       :inherit t :depth treeless :ref
			       "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (real-auto-save :source "elpaca-menu-lock-file" :recipe
		 (:package "real-auto-save" :fetcher github :repo
			   "ChillarAnand/real-auto-save" :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			    "*.texinfo" "doc/dir" "doc/*.info"
			    "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			    "docs/dir" "docs/*.info" "docs/*.texi"
			    "docs/*.texinfo"
			    (:exclude ".dir-locals.el" "test.el"
				      "tests.el" "*-test.el"
				      "*-tests.el" "LICENSE" "README*"
				      "*-pkg.el"))
			   :source "MELPA" :protocol https :inherit t
			   :depth treeless :ref
			   "50cd83db5682a8337fed5fa77045b2a65770f160"))
 (reformatter :source "elpaca-menu-lock-file" :recipe
	      (:package "reformatter" :repo
			"purcell/emacs-reformatter" :fetcher github
			:files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"c0ddac04b7b937ed56d6bf97e4bfcc4eccfa501a"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
	    (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher
		      github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "668069ad8b6ca20bd0d2334db1c0d046809affd6"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
	      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
	       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
	       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
	       "docs/*.texinfo"
	       (:exclude ".dir-locals.el" "test.el" "tests.el"
			 "*-test.el" "*-tests.el" "LICENSE" "README*"
			 "*-pkg.el"))
	      :source "MELPA" :protocol https :inherit t :depth
	      treeless :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (sbt-mode :source "elpaca-menu-lock-file" :recipe
	   (:package "sbt-mode" :fetcher github :repo
		     "hvesalai/emacs-sbt-mode" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		      "doc/*.texinfo" "lisp/*.el" "docs/dir"
		      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		      (:exclude ".dir-locals.el" "test.el" "tests.el"
				"*-test.el" "*-tests.el" "LICENSE"
				"README*" "*-pkg.el"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "c353df6aa112c05dde6dc63ccf813c2203cb472b"))
 (scala-mode :source "elpaca-menu-lock-file" :recipe
	     (:package "scala-mode" :fetcher github :repo
		       "hvesalai/emacs-scala-mode" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "50bcafa181baec7054e27f4bca55d5f9277c6350"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
	      (:package "shrink-path" :fetcher gitlab :repo
			"bennya/shrink-path.el" :files
			("*.el" "*.el.in" "dir" "*.info" "*.texi"
			 "*.texinfo" "doc/dir" "doc/*.info"
			 "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			 "docs/dir" "docs/*.info" "docs/*.texi"
			 "docs/*.texinfo"
			 (:exclude ".dir-locals.el" "test.el"
				   "tests.el" "*-test.el" "*-tests.el"
				   "LICENSE" "README*" "*-pkg.el"))
			:source "MELPA" :protocol https :inherit t
			:depth treeless :ref
			"c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (spinner :source "elpaca-menu-lock-file" :recipe
	  (:package "spinner" :repo
		    ("https://github.com/Malabarba/spinner.el"
		     . "spinner")
		    :files ("*" (:exclude ".git")) :source "GNU ELPA"
		    :protocol https :inherit t :depth treeless :ref
		    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (sql-indent :source "elpaca-menu-lock-file" :recipe
	     (:package "sql-indent" :repo "alex-hhh/emacs-sql-indent"
		       :files ("*" (:exclude ".git")) :source
		       "GNU ELPA" :protocol https :inherit t :depth
		       treeless :host github :ref
		       "2ed4c6a26b8f3d651ac6231eaafb2565d77c918b"))
 (tablist :source "elpaca-menu-lock-file" :recipe
	  (:package "tablist" :fetcher github :repo
		    "emacsorphanage/tablist" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (transient :source "elpaca-menu-lock-file" :recipe
	    (:package "transient" :fetcher github :repo
		      "magit/transient" :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "79c49830a80f550728738e378b7f7a53dd908478"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
	   (:package "treemacs" :fetcher github :repo
		     "Alexander-Miller/treemacs" :files
		     (:defaults "Changelog.org" "icons"
				"src/elisp/treemacs*.el"
				"src/scripts/treemacs*.py"
				(:exclude "src/extra/*"))
		     :source "MELPA" :protocol https :inherit t :depth
		     treeless :ref
		     "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treesit-auto :source "elpaca-menu-lock-file" :recipe
	       (:package "treesit-auto" :fetcher github :repo
			 "renzmann/treesit-auto" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			  "*.texinfo" "doc/dir" "doc/*.info"
			  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			  "docs/dir" "docs/*.info" "docs/*.texi"
			  "docs/*.texinfo"
			  (:exclude ".dir-locals.el" "test.el"
				    "tests.el" "*-test.el"
				    "*-tests.el" "LICENSE" "README*"
				    "*-pkg.el"))
			 :source "MELPA" :protocol https :inherit t
			 :depth treeless :ref
			 "31466e4ccfd4f896ce3145c95c4c1f8b59d4bfdf"))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		"doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		"lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		"docs/*.texinfo"
		(:exclude ".dir-locals.el" "test.el" "tests.el"
			  "*-test.el" "*-tests.el" "LICENSE" "README*"
			  "*-pkg.el"))
	       :source "MELPA" :protocol https :inherit t :depth
	       treeless :ref
	       "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (undo-fu :source "elpaca-menu-lock-file" :recipe
	  (:package "undo-fu" :fetcher codeberg :repo
		    "ideasman42/emacs-undo-fu" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		     "doc/*.texinfo" "lisp/*.el" "docs/dir"
		     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		     (:exclude ".dir-locals.el" "test.el" "tests.el"
			       "*-test.el" "*-tests.el" "LICENSE"
			       "README*" "*-pkg.el"))
		    :source "MELPA" :protocol https :inherit t :depth
		    treeless :ref
		    "5684ef2aef5f60176472916b21869cf221e018cc"))
 (vertico :source "elpaca-menu-lock-file" :recipe
	  (:package "vertico" :repo "minad/vertico" :files
		    (:defaults "extensions/vertico-*.el") :fetcher
		    github :source "MELPA" :protocol https :inherit t
		    :depth treeless :ref
		    "0b96e8f169653cba6530da1ab0a1c28ffa44b180"))
 (vterm :source "elpaca-menu-lock-file" :recipe
	(:package "vterm" :fetcher github :repo
		  "akermu/emacs-libvterm" :files
		  ("CMakeLists.txt" "elisp.c" "elisp.h"
		   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
		   "vterm-module.c" "vterm-module.h")
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :ref
		  "a01a2894a1c1e81a39527835a9169e35b7ec5dec"))
 (vue-ts-mode :source "elpaca-menu-lock-file" :recipe
	      (:source "Init file" :protocol https :inherit t :depth
		       treeless :host github :repo
		       "8uff3r/vue-ts-mode" :package "vue-ts-mode"
		       :ref "efc7031f50bbfd2a3293aee4fcb34bf0503b7f83"))
 (web-server :source "elpaca-menu-lock-file" :recipe
	     (:package "web-server" :fetcher github :repo
		       "eschulte/emacs-web-server" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
			"*.texinfo" "doc/dir" "doc/*.info"
			"doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			"docs/dir" "docs/*.info" "docs/*.texi"
			"docs/*.texinfo"
			(:exclude ".dir-locals.el" "test.el"
				  "tests.el" "*-test.el" "*-tests.el"
				  "LICENSE" "README*" "*-pkg.el"))
		       :source "MELPA" :protocol https :inherit t
		       :depth treeless :ref
		       "6357a1c2d1718778503f7ee0909585094117525b"))
 (websocket :source "elpaca-menu-lock-file" :recipe
	    (:package "websocket" :repo "ahyatt/emacs-websocket"
		      :fetcher github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
	(:package "wgrep" :fetcher github :repo
		  "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :ref
		  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (which-key :source "elpaca-menu-lock-file" :recipe
	    (:package "which-key" :repo "justbur/emacs-which-key"
		      :fetcher github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "38d4308d1143b61e4004b6e7a940686784e51500"))
 (winum :source "elpaca-menu-lock-file" :recipe
	(:package "winum" :fetcher github :repo "deb0ch/emacs-winum"
		  :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		   "doc/*.texinfo" "lisp/*.el" "docs/dir"
		   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		   (:exclude ".dir-locals.el" "test.el" "tests.el"
			     "*-test.el" "*-tests.el" "LICENSE"
			     "README*" "*-pkg.el"))
		  :source "MELPA" :protocol https :inherit t :depth
		  treeless :ref
		  "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
	     :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
	      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
	      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
	      "docs/*.texinfo"
	      (:exclude ".dir-locals.el" "test.el" "tests.el"
			"*-test.el" "*-tests.el" "LICENSE" "README*"
			"*-pkg.el"))
	     :source "MELPA" :protocol https :inherit t :depth
	     treeless :ref "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el"
			    "*-test.el" "*-tests.el" "LICENSE"
			    "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth
		 treeless :ref
		 "f2369fb4985ed054be47ae111760ff2075dff72a"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
	    (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
		      github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
		       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
		       "doc/*.texinfo" "lisp/*.el" "docs/dir"
		       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
		       (:exclude ".dir-locals.el" "test.el" "tests.el"
				 "*-test.el" "*-tests.el" "LICENSE"
				 "README*" "*-pkg.el"))
		      :source "MELPA" :protocol https :inherit t
		      :depth treeless :ref
		      "d91f878729312a6beed77e6637c60497c5786efa"))
 (zoom :source "elpaca-menu-lock-file" :recipe
       (:package "zoom" :repo "cyrus-and/zoom" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
		  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
		  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el"
			    "*-test.el" "*-tests.el" "LICENSE"
			    "README*" "*-pkg.el"))
		 :source "MELPA" :protocol https :inherit t :depth
		 treeless :ref
		 "36f9db90941b10d34bac976aee35dfe25242cd03")))
