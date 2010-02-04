Feature: Wrap Region
  In order to wrap text
  As a user
  I want to wrap text with different stuff

  Background:
    Given I am in the "*wrap-region*" buffer
    And the buffer is empty
    And the following contents:
      """
      This is some text
      """
    And there is no region selected

  Scenario: No wrap when wrap-region is inactive
    Given I "inactivate" "wrap-region" mode
    When I select "is some"
    And I press "("
    Then I should see "This (is some text"

  Scenario: Wrap region with punctuations
    Given I "activate" "wrap-region" mode
    When I select "is some"
    And I press "("
    Then I should see "This (is some) text"

  Scenario: Insert once
    Given insert twice is turned "off"
    And I "activate" "wrap-region" mode
    When I press "("
    Then I should see "("
    And I should not see "()"

  Scenario: Insert twice
    Given insert twice is turned "on"
    And I "activate" "wrap-region" mode
    When I press "("
    Then I should see "()"

  Scenario: Before hook
    And I "activate" "wrap-region" mode
    And the following before hook:
      """
      (forward-word)
      (transpose-words 1)
      """
    When I select "is some"
    And I press "("
    Then I should see "This (some is) text"

  Scenario: After hook
    And I "activate" "wrap-region" mode
    And the following after hook:
      """
      (replace-regexp "\\((.+)\\)" "(wee...)" nil (point-min) (point-max))
      """
    When I select "is some"
    And I press "("
    Then I should see "This (wee...) text"

  Scenario: Set specific punctuations
    Given the following is loaded
      """
      (add-hook 'text-mode-hook
            (lambda()
              (wrap-region-set-mode-punctuations '("["))
              ))
      """
    When I "activate" "text" mode
    And I select "is some"
    And I press "["
    Then I should see "This [is some] text"
    And I select "is some"
    And I press "<"
    Then I should see "This [<is some] text"
    And I should not see "This [<is some>] text"

  Scenario: Set specific punctuations by passing mode
    Given the following is loaded
      """
      (wrap-region-set-mode-punctuations '("[") 'text-mode)
      """
    When I "activate" "text" mode
    And I select "is some"
    And I press "["
    Then I should see "This [is some] text"
    And I select "is some"
    And I press "<"
    Then I should see "This [<is some] text"
    And I should not see "This [<is some>] text"

  Scenario: Add punctuation
    Given the following is loaded
      """
      (wrap-region-add-punctuation "#" "#")
      """
    And I "activate" "wrap-region" mode
    And I select "is some"
    And I press "["
    Then I should see "This [is some] text"
    And I select "is some"
    And I press "#"
    Then I should see "This [#is some#] text"

  # NOTE (For the tag scenarios below): Must make sure we are in the
  # *wrap-region* buffer since wrap-region-tag-active is buffer local.
    
  Scenario: Wrap region with tag
    Given I am in the "*wrap-region*" buffer
    And wrap-region tag is "active"
    And I "activate" "wrap-region" mode
    When I select "is some"
    And I start a kbd sequence
    And I press "<"
    And I type "div"
    And I press "RET"
    And I execute the action chain
    Then I should see "This <div>is some</div> text"

  Scenario: Wrap region with tag including attribute
    Given I am in the "*wrap-region*" buffer
    And wrap-region tag is "active"
    And I "activate" "wrap-region" mode
    When I select "is some"
    And I start a kbd sequence
    And I press "<"
    And I type "div id='menu'"
    And I press "RET"
    And I execute the action chain
    Then I should see "This <div id='menu'>is some</div> text"
