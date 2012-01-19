Feature: Add Wrappers
  In order to extend wrap region
  As a wrap region user
  I want to add wrappers

  Scenario: Add wrapper
    Given I add wrapper "#/#"
     Then key "#" should wrap with "#" and "#"

  Scenario: Add wrapper with custom trigger
    Given I add wrapper "{-/-}/#"
     Then key "#" should wrap with "{-" and "-}"

  Scenario: Add wrapper for mode
    Given I add wrapper "#/#" for "text-mode"
     Then key "#" should wrap with "#" and "#" in "text-mode"

  Scenario: Add wrapper with custom trigger for mode
    Given I add wrapper "{-/-}/#" for "text-mode"
     Then key "#" should wrap with "{-" and "-}" in "text-mode"

  Scenario: Add wrapper for modes
    Given I add wrapper "#/#" for "text-mode,fundamental-mode"
     Then key "#" should wrap with "#" and "#" in "text-mode"
      And key "#" should wrap with "#" and "#" in "fundamental-mode"
     
  Scenario: Add wrapper with custom trigger for modes
    Given I add wrapper "{-/-}/#" for "text-mode,fundamental-mode"
     Then key "#" should wrap with "{-" and "-}" in "text-mode"
      And key "#" should wrap with "{-" and "-}" in "fundamental-mode"

  Scenario: Override wrapper
    Given I add wrapper "$/#"
      And I add wrapper "$/$"
     Then key "$" should wrap with "$" and "$"
  
  Scenario: Override wrapper for mode
    Given I add wrapper "$/$" for "text-mode"
      And I add wrapper "#/#/$" for "text-mode"
     Then key "$" should wrap with "#" and "#" in "text-mode"

  Scenario: Add another wrapper for same mode
    Given I add wrapper "$/$" for "text-mode"
      And I add wrapper "#/#" for "text-mode"
     Then key "$" should wrap with "$" and "$" in "text-mode"
      And key "#" should wrap with "#" and "#" in "text-mode"
      
  Scenario: Add same wrapper for another mode
    Given I add wrapper "$/$" for "text-mode"
      And I add wrapper "$/$" for "fundamental-mode"
     Then key "$" should wrap with "$" and "$" in "text-mode"
      And key "$" should wrap with "$" and "$" in "fundamental-mode"

  Scenario: Add wrapper with custom trigger when mode and other mode already has wrapper with that trigger
    Given I add wrapper "$/$" for "text-mode,fundamental-mode"
      And I add wrapper "$/$"
     Then key "$" should wrap with "$" and "$" in "fundamental-mode"
      And key "$" should wrap with "$" and "$" in "text-mode"
      And key "$" should wrap with "$" and "$" in "emacs-lisp-mode"

  Scenario: Add wrapper with custom trigger for mode when mode and other mode already has wrapper with that trigger
    Given I add wrapper "$/$" for "text-mode,fundamental-mode"
      And I add wrapper "#/#/$" for "text-mode"
     Then key "$" should wrap with "$" and "$" in "fundamental-mode"
      And key "$" should wrap with "#" and "#" in "text-mode"

  Scenario: Add wrapper with custom trigger for mode when already exist global wrapper with that trigger
    Given I add wrapper "$/$"
      And I add wrapper "#/#/$" for "text-mode"
     Then key "$" should wrap with "$" and "$" in "fundamental-mode"
      And key "$" should wrap with "#" and "#" in "text-mode"

  Scenario: Add wrapper with custom trigger for modes when one of the modes already has wrapper with that trigger (single mode exist)
    Given I add wrapper "$/$" for "text-mode"
      And I add wrapper "#/#/$" for "text-mode,fundamental-mode"
     Then key "$" should wrap with "#" and "#" in "text-mode"
      And key "$" should wrap with "#" and "#" in "fundamental-mode"

  Scenario: Add wrapper with custom trigger for modes when one of the modes already has wrapper with that trigger (multiple modes exists)
    Given I add wrapper "$/$" for "text-mode,fundamental-mode"
      And I add wrapper "#/#/$" for "text-mode,emacs-lisp-mode"
     Then key "$" should wrap with "#" and "#" in "text-mode"
     Then key "$" should wrap with "#" and "#" in "emacs-lisp-mode"
      And key "$" should wrap with "$" and "$" in "fundamental-mode"

  Scenario: Add wrapper when mode already has wrapper with that trigger
    Given I add wrapper "$/$" for "text-mode"
      And I add wrapper "#/#/$"
     Then key "$" should wrap with "$" and "$" in "text-mode"
     Then key "$" should wrap with "#" and "#" in "emacs-lisp-mode"
