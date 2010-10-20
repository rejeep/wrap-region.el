Feature: Hooks
  In order to more easily customize wrap region
  As an Emacs user
  I want to use it's hooks

  Scenario: Mode hook
    Given I add this mode hook
      """
      (insert "some text")
      """
    When I enable wrap-region
    Then I should see "some text"
    
  Scenario: Before wrap hook
    Given I add this before wrap hook
      """
      (replace-regexp "is some" "some is" nil (point-min) (point-max))
      """
    When I enable wrap-region
    And I insert "this is some text"
    And I select "is some"
    And I press "("
    Then I should see "this (some is) text"
    
  Scenario: After wrap hook
    Given I add this after wrap hook
      """
      (replace-regexp "(\\(.+\\))" "[\\1]" nil (point-min) (point-max))
      """
    When I enable wrap-region
    And I insert "this is some text"
    And I select "is some"
    And I press "("
    Then I should see "this [is some] text"
