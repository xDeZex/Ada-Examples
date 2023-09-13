with AUnit.Assertions; use AUnit.Assertions;
package body Building.Tests is

   Manager : Building_Manager_Access;
   
   procedure Construct_Building_Manager_For_Tests (Manager : out Building_Manager_Access);

   procedure Is_Elevator_On_Correct_Floor (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Manager.Elevator.Which_Floor_Is_Elevator_On = 1, "Elevator starts on wrong floor");

      Manager.Elevator.Move_Elevator_To_Floor (2);

      Assert (Manager.Elevator.Which_Floor_Is_Elevator_On = 2, "Elevator moves to wrong floor");
   end Is_Elevator_On_Correct_Floor;

   procedure Is_The_Correct_Amount_of_People_On_The_Elevator (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Manager.Elevator.How_Many_People_In_Elevator = 0, "Elevator start with wrong number of people in it");
      
      Manager.Elevator.Person_Enter_Elevator;

      Assert (Manager.Elevator.How_Many_People_In_Elevator = 1, "Person doesn't enter elevator");
   end Is_The_Correct_Amount_of_People_On_The_Elevator;

   procedure Does_Person_Step_On (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Manager.Elevator.Person_Enter_Elevator;
      Assert (Manager.Elevator.How_Many_People_In_Elevator = 1, "Person doesn't enter elevator");
   end Does_Person_Step_On;

   procedure Does_Person_Step_Off (T : in out Test_Cases.Test_Case'Class) is     
      pragma Unreferenced (T);
   begin
      Manager.Elevator.Person_Enter_Elevator;
      Manager.Elevator.Move_Elevator_To_Floor (2);

      Manager.Elevator.Person_Leave_Elevator;

      Assert (Manager.Elevator.How_Many_People_In_Elevator = 0, "Person doesn't leave elevator");
      Assert (Manager.Elevator.How_Many_People_Are_On_The_Correct_Floor = 1, "Person leaves elevator but isn't on correct floor");
   end Does_Person_Step_Off;

   procedure Does_Person_Not_Step_Off_On_Wrong_Floor (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Manager.Elevator.Person_Enter_Elevator;
      Manager.Elevator.Move_Elevator_To_Floor (3);

      Manager.Elevator.Person_Leave_Elevator;
      
      Assert (Manager.Elevator.How_Many_People_In_Elevator = 1, "Person leaves elevator on wrong floor");
      Assert (Manager.Elevator.How_Many_People_Are_On_The_Correct_Floor = 0, "Person doesn't leave elevator but is on correct floor");
   end Does_Person_Not_Step_Off_On_Wrong_Floor;

   procedure Does_Person_Not_Step_On_On_Wrong_Floor (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Manager.Elevator.Move_Elevator_To_Floor (2);

      Manager.Elevator.Person_Enter_Elevator;

      Assert (Manager.Elevator.How_Many_People_In_Elevator = 0, "Person enters elevator on wrong floor");
   end Does_Person_Not_Step_On_On_Wrong_Floor;


   procedure Does_Elevator_Show_Where_People_Are_Going (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Manager.Elevator.Where_People_Are_Going_From_Floor (1) (0) = 2, "Doesn't show where people are going correctly on floor 1");
      Assert (Manager.Elevator.Where_People_Are_Going_From_Floor (1)'Length = 1, "Returned array is not the right length on floor 1");

      Assert (Manager.Elevator.Where_People_Are_Going_From_Floor (3) (0) = 4, "Doesn't show where person 1 are going correctly on floor 3");
      Assert (Manager.Elevator.Where_People_Are_Going_From_Floor (3) (1) = 1, "Doesn't show where person 2 are going correctly on floor 3");
      Assert (Manager.Elevator.Where_People_Are_Going_From_Floor (3)'Length = 2, "Returned array is not the right length on floor 3");
   end Does_Elevator_Show_Where_People_Are_Going;

   overriding
   procedure Register_Tests (T : in out Elevator_Tests) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (T, Is_Elevator_On_Correct_Floor'Access, "Test Move Floor");
      Register_Routine (T, Is_The_Correct_Amount_of_People_On_The_Elevator'Access, "Test No people on elevator at start");
      Register_Routine (T, Does_Person_Step_On'Access, "Test if person steps on");
      Register_Routine (T, Does_Person_Step_Off'Access, "Test if person steps off");
      Register_Routine (T, Does_Person_Not_Step_Off_On_Wrong_Floor'Access, "Test if person steps off at wrong floor");
      Register_Routine (T, Does_Person_Not_Step_On_On_Wrong_Floor'Access, "Test if person steps on at wrong floor");
      Register_Routine (T, Does_Elevator_Show_Where_People_Are_Going'Access, "Test if elevator shows where people are going");
   end Register_Tests;

   ----------------------------------------------------------------
   -- The setup for testing: 
   -- Floor: Floor_To_Exit_At, Floor_To_Exit_At, and so on
   -- 10:
   -- 9:
   -- 8:
   -- 7:
   -- 6:
   -- 5:
   -- 4:
   -- 3: 4, 1
   -- 2:
   -- 1: 2
   ---------------------------------------------------------------
   procedure Add_People_To_Building_For_Tests (Manager : Building_Manager_Access) is
      Person : People;
   begin
      Person := (2, 1, 1);
      Manager.Elevator.Add_Person (Person, 1);
      Person := (4, 3, 3);
      Manager.Elevator.Add_Person (Person, 3);
      Person := (1, 3, 3);
      Manager.Elevator.Add_Person (Person, 3);
   end Add_People_To_Building_For_Tests;

      -- The same thing. But a specific setup for testing.
   procedure Construct_Building_Manager_For_Tests (Manager : out Building_Manager_Access) is
      Elevator : constant Elevator_Access := new Building.Elevator;
      Manager1 : constant Building_Manager_Access := new Building_Manager (Elevator);
   begin
      Time_Unit := Duration (0.0001);
      Time_To_Enter := Time_Unit * 1;
      Time_To_Exit := Time_Unit * 1;
      Time_To_Move_A_Floor := Time_Unit * 3;
      Add_People_To_Building_For_Tests (Manager1);
      Manager := Manager1;
   end Construct_Building_Manager_For_Tests;

   overriding
   procedure Set_Up (T : in out Elevator_Tests) is
   begin
      Time_Unit := Duration (0);
      Construct_Building_Manager_For_Tests (Manager);
   end Set_Up;

   overriding
   function Name (T : Elevator_Tests) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Elevator Tests");
   end Name;

end Building.Tests;