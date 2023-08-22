with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
package Building.Tests is
   type Elevator_Tests is new Test_Cases.Test_Case with null record;

   overriding
   procedure Register_Tests (T : in out Elevator_Tests);

   overriding
   function Name (T : Elevator_Tests) return Message_String;
   -- Provide name identifying the test case

   overriding
   procedure Set_Up (T : in out Elevator_Tests);
   -- Test Routines:
   procedure Is_Elevator_On_Correct_Floor (T : in out Test_Cases.Test_Case'Class);
   procedure Is_The_Correct_Amount_of_People_On_The_Elevator (T : in out Test_Cases.Test_Case'Class);
   procedure Does_Person_Step_On (T : in out Test_Cases.Test_Case'Class);
   procedure Does_Person_Step_Off (T : in out Test_Cases.Test_Case'Class);
   procedure Does_Person_Not_Step_Off_On_Wrong_Floor (T : in out Test_Cases.Test_Case'Class);
   procedure Does_Person_Not_Step_On_On_Wrong_Floor (T : in out Test_Cases.Test_Case'Class);
   procedure Does_Elevator_Show_Where_People_Are_Going (T : in out Test_Cases.Test_Case'Class);
   
private
    
end Building.Tests;