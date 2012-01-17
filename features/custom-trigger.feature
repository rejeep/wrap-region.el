Feature: Custom trigger
  In order to change trigger
  As a wrap region user
  I want to add wrapper with custom trigger

  Background:
    Given I enable wrap-region

  Scenario: Add custom trigger and then wrap
    Given I add wrapper "{-/-}/#"
     When I insert "This is some text"
      And I select "is some"
      And I press "#"
     Then I should see "This {-is some-} text"

  Scenario: Add custom trigger and then wrap with no region selected
    Given I add wrapper "{-/-}/#"
     When I insert "This is some text"
     When I press "#"
     Then I should not see "This {-is some-} text"
      But I should see "This is some text#"

  Scenario: Remove wrapper
    Given I add wrapper "{-/-}/#"
     When I insert "This is some text"
      And I select "is some"
      And I press "#"
     Then I should see "This {-is some-} text"
    Given I remove wrapper "#"
      And I select "is some"
      And I press "#"
     Then I should not see "This {-{-is some-}-} text"
      But I should see "This {-#is some-} text"

  Scenario: Override existing wrapper
    Given I add wrapper "#/#"
     When I insert "This is some text"
      And I select "is some"
      And I press "#"
     Then I should see "This #is some# text"
    Given I add wrapper "{-/-}/#"
      And I select "is some"
      And I press "#"
     Then I should see "This #{-is some-}# text"
