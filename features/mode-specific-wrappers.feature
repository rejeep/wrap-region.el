Feature: Mode Specific Wrappers
  As a wrap region user
  I want to add mode specific wrappers

  Scenario: Add simple wrapper for single mode
    Given I add wrapper "$/$" for "latex-mode"
     When I enable fundamental-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "$"
     Then I should not see "This $is some$ text"
      But I should see "This $is some text"
     When I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "$"
     Then I should see "This $$is some$ text"

  Scenario: Add another simple wrapper for same mode
    Given I add wrapper "$/$" for "latex-mode"
      And I add wrapper "$/$" for "text-mode"
     When I enable fundamental-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "$"
     Then I should not see "This $is some$ text"
      But I should see "This $is some text"
     When I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "$"
     Then I should see "This $$is some$ text"
     When I enable text-mode
      And I enable wrap-region
      And I select "is some"
      And I press "$"
     Then I should see "This $$$is some$$ text"

  Scenario: Add simple wrapper for multiple modes
    Given I add wrapper "$/$" for "latex-mode,text-mode"
     When I enable fundamental-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "$"
     Then I should not see "This $is some$ text"
      But I should see "This $is some text"
     When I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "$"
     Then I should see "This $$is some$ text"
     When I enable text-mode
      And I enable wrap-region
      And I select "is some"
      And I press "$"
     Then I should see "This $$$is some$$ text"

  Scenario: Add custom trigger wrapper for single mode
    Given I add wrapper "{-/-}/#" for "latex-mode"
     When I enable fundamental-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "#"
     Then I should not see "This {-is some-} text"
      But I should see "This #is some text"
     When I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "#"
     Then I should see "This #{-is some-} text"

  Scenario: Add another custom trigger wrapper for same mode
    Given I add wrapper "{-/-}/#" for "latex-mode"
      And I add wrapper "{-/-}/#" for "text-mode"
     When I enable fundamental-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "#"
     Then I should not see "This {-is some-} text"
      But I should see "This #is some text"
     When I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "#"
     Then I should see "This #{-is some-} text"
     When I enable text-mode
      And I enable wrap-region
      And I select "is some"
      And I press "#"
     Then I should see "This #{-{-is some-}-} text"

  Scenario: Add custom trigger wrapper for multiple modes
    Given I add wrapper "{-/-}/#" for "latex-mode,text-mode"
     When I enable fundamental-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "#"
     Then I should not see "This {-is some-} text"
      But I should see "This #is some text"
     When I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "#"
     Then I should see "This #{-is some-} text"
     When I enable text-mode
      And I enable wrap-region
      And I select "is some"
      And I press "#"
     Then I should see "This #{-{-is some-}-} text"

   Scenario: Remove simple wrapper for single mode
     Given I add wrapper "$/$" for "latex-mode"
       And I enable latex-mode
       And I enable wrap-region
       And I insert "This is some text"
       And I select "is some"
       And I press "$"
      Then I should see "This $is some$ text"
      When I remove wrapper "$" from "latex-mode"
       And I select "is some"
       And I press "$"
      Then I should not see "This $$is some$$ text"
       But I should see "This $$is some$ text"

   Scenario: Remove simple wrapper for multiple modes
     Given I add wrapper "$/$" for "latex-mode,text-mode"
       And I enable latex-mode
       And I enable wrap-region
       And I insert "This is some text"
       And I select "is some"
       And I press "$"
      Then I should see "This $is some$ text"
      When I enable text-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should see "This $$is some$$ text"
      When I remove wrapper "$" from "latex-mode,text-mode"
       And I enable latex-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should not see "This $$$is some$$$ text"
       But I should see "This $$$is some$$ text"
      When I enable text-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should not see "This $$$$is some$$$ text"
       But I should see "This $$$$is some$$ text"

   Scenario: Remove single simple wrapper when multiple modes
     Given I add wrapper "$/$" for "latex-mode,text-mode"
       And I insert "This is some text"
       And I enable latex-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should see "This $is some$ text"
      When I enable text-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should see "This $$is some$$ text"
      When I remove wrapper "$" from "latex-mode"
       And I enable text-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should see "This $$$is some$$$ text"
      When I enable latex-mode
       And I enable wrap-region
       And I select "is some"
       And I press "$"
      Then I should not see "This $$$$is some$$$$ text"
       But I should see "This $$$$is some$$$ text"

   Scenario: Remove custom trigger wrapper for single mode
     Given I add wrapper "{-/-}/#" for "latex-mode"
       And I enable latex-mode
       And I enable wrap-region
       And I insert "This is some text"
       And I select "is some"
       And I press "#"
      Then I should see "This {-is some-} text"
      When I remove wrapper "#" from "latex-mode"
       And I select "is some"
       And I press "#"
      Then I should not see "This {-{-is some-}-} text"
       But I should see "This {-#is some-} text"

   Scenario: Remove custom-trigger wrapper for multiple modes
     Given I add wrapper "{-/-}/#" for "latex-mode,text-mode"
       And I enable latex-mode
       And I enable wrap-region
       And I insert "This is some text"
       And I select "is some"
       And I press "#"
      Then I should see "This {-is some-} text"
      When I enable text-mode
       And I enable wrap-region
       And I select "is some"
       And I press "#"
      Then I should see "This {-{-is some-}-} text"
      When I remove wrapper "#" from "latex-mode"
      When I enable latex-mode
       And I enable wrap-region
       And I select "is some"
       And I press "#"
      Then I should not see "This {-{-{-is some-}-}-} text"
       But I should see "This {-{-#is some-}-} text"
       
  Scenario: Remove simple wrapper for non mode-specific
    Given I add wrapper "$/$"
      And I enable latex-mode
      And I enable wrap-region
      And I insert "This is some text"
      And I select "is some"
      And I press "$"
     Then I should see "This $is some$ text"
     When I remove wrapper "$" from "latex-mode"
      And I enable latex-mode
      And I enable wrap-region
      And I select "is some"
      And I press "$"
     Then I should see "This $$is some$$ text"
