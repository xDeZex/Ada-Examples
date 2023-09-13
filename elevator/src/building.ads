with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;
package Building is
   subtype Floor_Range is Positive range Positive'First .. 10;
   type People is record
      Floor_To_Exit_At : Floor_Range;
      Floor_To_Enter_At : Floor_Range;
      Floor_At : Floor_Range;
   end record;

   Time_Unit : Duration := Duration (0.01);
   Time_To_Enter : Duration := Time_Unit * 1;
   Time_To_Exit : Duration := Time_Unit * 1;
   Time_To_Move_A_Floor : Duration := Time_Unit * 3;
   Max_People_On_Each_Floor : Positive := 10;

   Top_Floor : Floor_Range := Floor_Range'Last;

   type Floor_Array is array (Natural range <>) of Floor_Range;

   package People_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
      Element_Type => People);
   
   type People_Outside_Elevator_Array is array (1 .. Top_Floor) of People_Vectors.Vector;

   --  The declaration of the biggest "class". Notice how the variables are private and only available through the subprograms.
   protected type Elevator is
      function Which_Floor_Is_Elevator_On return Floor_Range;
      function How_Many_People_In_Elevator return Natural;
      function How_Many_People_Going_To_Floor_In_Elevator (Floor_In_Question : Floor_Range) return Natural;
      function Where_People_Are_Going_From_Floor (From_Floor : Floor_Range) return Floor_Array;
      function Where_People_Are_Going_In_Elevator return Floor_Array;
      function How_Many_People_Waiting_On_Floor (Floor_In_Question : Floor_Range) return Natural;
      function Is_Moving return Boolean;
      function How_Many_People_Are_On_The_Correct_Floor return Natural;
      function How_Many_People_Are_On_The_Wrong_Floor return Natural;
      procedure Person_Leave_Elevator;
      procedure Person_Enter_Elevator;
      procedure Add_Person (Person : People; To_Floor : Floor_Range);
      procedure Print_Elevator;
      procedure Close_Elevator;
      entry Move_Elevator_To_Floor (Floor_Going_To : Floor_Range);
   private
      Moving : Boolean := False;
      People_In_Elevator : People_Vectors.Vector;
      People_Outside_Elevator : People_Outside_Elevator_Array;
      Floor : Floor_Range := 1;
      Started : Boolean := False;
      Ended : Boolean := False;
      Start_Time : Time;
      End_Time : Time;
   end Elevator;
   type Elevator_Access is access all Elevator;

   task type Building_Manager (Elevator : Elevator_Access) is
      entry Go_To_Floor (Floor_Going_To : Floor_Range);
      entry Print_Elevator;
      entry Close_Elevator;
   end Building_Manager;
   type Building_Manager_Access is access all Building_Manager;

   procedure Construct_Building_Manager (Manager : out Building_Manager_Access; Seed : Integer := Integer'First);

   procedure Do_Not_Print;
   procedure Do_Print;

   Used_Time_Units : Natural := 0;
private
   Is_Printing : Boolean := False;
   procedure Add_People_To_Building (Manager : Building_Manager_Access; Seed : Integer := Integer'First);
   task type People_Mover (Elevator : Elevator_Access) is
      entry Move_People (Floor : Floor_Range);
   end People_Mover;
   type People_Mover_Access is access all People_Mover;
end Building;