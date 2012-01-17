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
     Then I should not see "This (is some) text"
      But I should see "This (is some text"

  Scenario: Wrap region
    When I insert "This is some text"
     And I select "is some"
     And I press "("
    Then I should see "This (is some) text"

  Scenario: Fallback when no region selected
    Given there is no region selected
     When I insert "This is some text"
      And I press "("
     Then I should see "This is some text("
      And I should not see "()"

  Scenario: Add wrapper
    Given I add wrapper "$/$"
     When I insert "this is some text"
      And I select "is some"
      And I press "$"
     Then I should see "this $is some$ text"

  Scenario: Remove wrapper
    Given I add wrapper "$/$"
     When I insert "this is some text"
      And I select "is some"
      And I press "$"
     Then I should see "this $is some$ text"
     When I remove wrapper "$"
      And I select "is some"
      And I press "$"
     Then I should not see "this $$is some$$ text"
      But I should see "this $$is some$ text"

  Scenario: Global mode
    Given I enable wrap-region globaly
     When I open temp file "global"
      And I insert "This is some text"
      And I select "is some"
      And I press "("
     Then I should see "(is some)"

  Scenario: Except modes
    Given I enable wrap-region globaly
      And I add text-mode as an except mode
     When I open temp file "global"
      And I enable text-mode
      And I insert "this is some text"
      And I select "is some"
      And I press "("
     Then I should not see "this (is some) text"
      But I should see "this (is some text"
