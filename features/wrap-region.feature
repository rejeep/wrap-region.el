Feature: Wrap Region
  In order to put text between puctuations and tags
  As an Emacs user
  I want to wrap it

  Background:
    Given wrap-region is active
    
  Scenario: No wrap when wrap-region is inactive
    Given wrap-region is inactive
    When I insert "This is some text"
    And I select "is some"
    And I press "("
    Then I should see "This (is some text"

  Scenario: Wrap region
    When I insert "This is some text"
    And I select "is some"
    And I press "("
    Then I should see "This (is some) text"

  Scenario: Fallback when no region selected
    When I press "("
    Then I should see "("
    And I should not see "()"

  Scenario: Add wrapper
    Given I add wrapper "`/`"
    When I insert "this is some text"
    And I select "is some"
    And I press "`"
    Then I should see "this `is some` text"
    
  Scenario: Remove wrapper
    Given I remove wrapper "("
    When I insert "this is some text"
    And I select "is some"
    And I press "("
    Then I should see "this (is some text"
    
  Scenario: Wrap with tag
    Given I enable html-mode
    And wrap-region is active
    When I insert "this is some text"
    And I select "is some"
    And I start an action chain
    And I press "<"
    And I type "div"
    And I execute the action chain
    Then I should see "<div>is some</div>"

  Scenario: Wrap with tag incuding attribute
    Given I enable html-mode
    And wrap-region is active
    When I insert "this is some text"
    And I select "is some"
    And I start an action chain
    And I press "<"
    And I type "div class='some-class'"
    And I execute the action chain
    Then I should see "<div class='some-class'>is some</div>"

  Scenario: Global mode
    Given I enable the global mode
    When I open temp file "global"
    And I insert "This is some text"
    And I select "is some"
    And I press "("
    Then I should see "(is some)"

  Scenario: Except modes
    Given I add text-mode as except mode
    And I enable the global mode
    When I open temp file "global"
    And I enable text-mode
    And I insert "this is some text"
    And I select "is some"
    And I press "("
    Then I should not see "this (is some) text"
    But I should see "this (is some text"
