Feature: Wrap with tags
  In order to make wrap region more useful in tag modes
  As a wrap region user
  I want to wrap region with tags

  Background:
    When I insert "this is some text"
     And I select "is some"

  Scenario: No tag wrap when no tag mode
    Given I enable text-mode
      And I enable wrap-region
      And I start an action chain
      And I press "<"
      And I type "div"
      And I execute the action chain
     Then I should not see "this <div>is some</div> text"
      But I should see "this div<is some> text"

  Scenario: Wrap with tag
    Given I enable html-mode
      And I enable wrap-region
      And I start an action chain
      And I press "<"
      And I type "div"
      And I execute the action chain
     Then I should see "<div>is some</div>"

  Scenario: Wrap with tag incuding attribute
    Given I enable html-mode
      And I enable wrap-region
      And I start an action chain
      And I press "<"
      And I type "div class=\"some-class\""
      And I execute the action chain
     Then I should see "<div class=\"some-class\">is some</div>"
