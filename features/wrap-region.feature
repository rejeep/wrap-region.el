Feature: Wrap Region
  In order to put text between puctuations and tags
  As an Emacs user
  I want to wrap it

  Background:
    Given I am in buffer "*wrap-region*"
    And the buffer is empty
    And I insert "This is some text"
    And transient mark mode is active
    And there is no region selected

  Scenario: No wrap when wrap-region is inactive
    Given I disable wrap-region
    When I select "is some"
    And I press "("
    Then I should see "This (is some text"

  Scenario: Wrap region with punctuations
    Given I enable wrap-region
    When I select "is some"
    And I press "("
    Then I should see "This (is some) text"

  Scenario: Insert once
    Given insert twice is inactive
    When I enable wrap-region
    And I press "("
    Then I should see "("
    And I should not see "()"

  Scenario: Insert twice
    Given insert twice is active
    When I enable wrap-region
    And I press "("
    Then I should see "()"

  Scenario: Insert twice delete
    Given insert twice is active
    When I enable wrap-region
    And I press "("
    Then I should see "()"
    When I press "DEL"
    Then I should not see "()"

  Scenario: Insert twice right buddy
    Given insert twice is active
    When I enable wrap-region
    And I press "("
    Then I should see "()"
    When I press ")"
    Then I should see "()"
    And I should not see "())"

  # # TODO: Not working
  # Scenario: Insert twice focus
  #   Given insert twice is active
  #   When I enable wrap-region
  #   And the buffer is empty
  #   And I press "("
  #   Then I should see "()"
  #   When I move point
  #   And I press ")"
  #   Then I should see "())"

  Scenario: Add punctuation
    When I load the following:
      """
      (wrap-region-add-punctuation "#" "#")
      """
    And I enable wrap-region
    And I select "is some"
    And I press "["
    Then I should see "This [is some] text"
    When I select "is some"
    And I press "#"
    Then I should see "This [#is some#] text"

  Scenario: Global mode
    When I load the following:
      """
      (wrap-region-global-mode t)
      """
    And I open temp file "global"
    And I insert "This is some text"
    And I select "is some"
    And I press "("
    Then I should see "(is some)"

  Scenario: Global mode except
    When I load the following:
      """
      (setq wrap-region-except-modes '(text-mode))
      (wrap-region-global-mode t)
      """
    And I am in buffer "*global-except*"
    And I insert:
      """
      This is some text
      """
    And I select "is some"
    And I press "("
    Then I should not see "(is some)"

  Scenario: Wrap Region hook
    When I load the following:
      """
      (add-hook 'wrap-region-hook
          (lambda ()
            (replace-regexp "is some" "some is" nil (point-min) (point-max))))
      """
    Then I should see "This is some text"
    When I enable wrap-region
    Then I should not see "This is some text"
    But I should see "This some is text"

  Scenario: Before insert twice hook
    Given insert twice is active
    When I load the following:
      """
      (add-hook 'wrap-region-before-insert-twice-hook 'erase-buffer)
      """
    And I enable wrap-region
    And I press "("
    Then I should see pattern "^()$"

  Scenario: After insert twice hook
    Given insert twice is active
    When I load the following:
      """
      (add-hook 'wrap-region-after-insert-twice-hook 'erase-buffer)
      """
    And I enable wrap-region
    And I press "("
    Then I should see pattern "^$"

  Scenario: Before wrap hook
    When I load the following:
      """
      (add-hook 'wrap-region-before-wrap-hook
          (lambda ()
            (replace-regexp "is some" "some is" nil (point-min) (point-max))))
      """
    And I enable wrap-region
    When I select "is some"
    And I press "("
    Then I should see "This (some is) text"

  Scenario: After wrap hook
    When I load the following:
      """
      (add-hook 'wrap-region-after-wrap-hook
          (lambda ()
            (replace-regexp "(\\(.+\\))" "[\\1]" nil (point-min) (point-max))))
      """
    And I enable wrap-region
    When I select "is some"
    And I press "("
    Then I should see "This [is some] text"

  Scenario: Wrap with tag
    Given I start html-mode
    And I enable wrap-region
    And I select "is some"
    And I start an action chain
    And I press "<"
    And I type "div"
    And I execute the action chain
    Then I should see "<div>is some</div>"

  Scenario: Wrap with tag incuding attribute
    Given I start html-mode
    And I enable wrap-region
    And I select "is some"
    And I start an action chain
    And I press "<"
    And I type "div class='some-class'"
    And I execute the action chain
    Then I should see "<div class='some-class'>is some</div>"
