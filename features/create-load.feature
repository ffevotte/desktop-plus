Feature: Create and load sessions

  Scenario: Create, load, create
    Given I am in a fresh Emacs instance

    When I call M-x "desktop+-create" RET "named-session" RET
    Then Desktop session "named-session" should exist

    Given I start an action chain
    And     I press "C-x C-f"
    And     I type "/tmp/foo"
    And     I execute the action chain
    And   I type "something"
    And   I press "C-x C-s"

    Given I am in a fresh Emacs instance
    When I call M-x "desktop+-load" RET "named-session" RET
    Then Buffer "foo" should exist

    Given I switch to buffer "foo"
    And   I call "emacs-lisp-mode"
    And   I call M-x "desktop+-create" RET "new-session" RET
    And   I call M-x "kill-buffer" RET "foo" RET
    When I call M-x "desktop+-load" RET "named-session" RET
    Then Buffer "foo" should exist

    When I switch to buffer "foo"
    Then Variable "major-mode" should be "emacs-lisp-mode"
