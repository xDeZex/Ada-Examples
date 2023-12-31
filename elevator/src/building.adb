with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Log; use Log;
package body Building is
   protected body Elevator is

      function How_Many_People_In_Elevator return Natural is
      begin
         return Integer (People_In_Elevator.Length);
      end How_Many_People_In_Elevator;

      function Which_Floor_Is_Elevator_On return Floor_Range is
      begin
         return Floor;
      end Which_Floor_Is_Elevator_On;

      --  Entry that changes floors on the elevator after a delay
      --  Will lock the elevator to moving during
      --  The compiler gives a warning when you use a delay in a protected,
      --  but I don't know how to move it without making the structure worse.
      --  This could be changed to a procedure, but you lose the blocking.
      entry Move_Elevator_To_Floor (Floor_Going_To : Floor_Range) when not Moving is
      begin
         Print (Is_Printing, "Started Moving");
         if not Started then
            Started := True;
            Start_Time := Clock;
         end if; 
         Moving := True;
         delay abs (Floor - Floor_Going_To) * Time_To_Move_A_Floor;
         Used_Time_Units := Used_Time_Units + Natural (abs (Floor - Floor_Going_To) * 3);
         Floor := Floor_Going_To;
         Moving := False;
      end Move_Elevator_To_Floor;

      function How_Many_People_Going_To_Floor_In_Elevator (Floor_In_Question : Floor_Range) return Natural is
      begin
         declare
            Number_Of_People_Going_To_Floor : Natural := 0;
         begin
            for Person of People_In_Elevator loop
               if Person.Floor_To_Exit_At = Floor_In_Question then
                  Number_Of_People_Going_To_Floor := Number_Of_People_Going_To_Floor + 1;
               end if;
            end loop;
            return Number_Of_People_Going_To_Floor;
         end;
      end How_Many_People_Going_To_Floor_In_Elevator;

      function Where_People_Are_Going_From_Floor (From_Floor : Floor_Range) return Floor_Array is
         Return_Array : Floor_Array (0 .. Integer (People_Outside_Elevator (From_Floor).Length) - 1);
      begin
         for I in Return_Array'Range loop
            Return_Array (I) := People_Outside_Elevator (From_Floor) (I).Floor_To_Exit_At;
         end loop;
         return Return_Array;
      end Where_People_Are_Going_From_Floor;

      function Where_People_Are_Going_In_Elevator return Floor_Array is
         Return_Array : Floor_Array (0 .. Integer (People_In_Elevator.Length) - 1);
      begin
         for I in Return_Array'Range loop
            Return_Array (I) := People_In_Elevator (I).Floor_To_Exit_At;
         end loop;
         return Return_Array;
      end Where_People_Are_Going_In_Elevator;

      function How_Many_People_Waiting_On_Floor (Floor_In_Question : Floor_Range) return Natural is
      begin
         declare
            Number_Of_People_Waiting_On_Floor : Natural := 0;
         begin
            for Person of People_Outside_Elevator (Floor_In_Question) loop
               if Person.Floor_To_Enter_At = Floor_In_Question then
                  Number_Of_People_Waiting_On_Floor := Number_Of_People_Waiting_On_Floor + 1;
               end if;
            end loop;
            return Number_Of_People_Waiting_On_Floor;
         end;
      end How_Many_People_Waiting_On_Floor;

      function Is_Moving return Boolean is
      begin
         return Moving;
      end Is_Moving;

      procedure Add_Person (Person : People; To_Floor : Floor_Range) is
      begin
         People_Outside_Elevator (To_Floor).Append (Person);
      end Add_Person;

      --  If there is at least one person on the elevator that exits on the current floor, then one person will exit
      procedure Person_Leave_Elevator is
      begin
         if not People_In_Elevator.Is_Empty then
            for i in 0 .. Integer (People_In_Elevator.Length) - 1 loop
               if People_In_Elevator (i).Floor_To_Exit_At = Floor then
                  declare
                     temp : People := People_In_Elevator (i);
                  begin
                     temp.Floor_At := Floor;
                     People_Outside_Elevator (Floor).Append (temp);
                     People_In_Elevator.Delete (i);
                     Print (Is_Printing, "Person exited to floor:" & Floor'Image);
                     exit;
                  end;
               end if;
            end loop;
         end if;
      end Person_Leave_Elevator;

      --  If there is at least one person on the elevator that enters on the current floor, then one person will enter
      procedure Person_Enter_Elevator  is
      begin
         if not People_Outside_Elevator (Floor).Is_Empty then
            for i in 0 .. Integer (People_Outside_Elevator (Floor).Length) - 1 loop
               if People_Outside_Elevator (Floor) (i).Floor_To_Enter_At = Floor then
                  People_In_Elevator.Append (People_Outside_Elevator (Floor) (i));
                  People_Outside_Elevator (Floor).Delete (i);
                  Print (Is_Printing, "Person entered from floor:" & Floor'Image);
                  exit;
               end if;
            end loop;
         end if;
      end Person_Enter_Elevator;

      function How_Many_People_Are_On_The_Correct_Floor return Natural is
      begin
         declare
            Number_Of_People_On_The_Correct_Floor : Natural := 0;
         begin
            for F in Floor_Range loop
               for Person of People_Outside_Elevator (F) loop
                  if Person.Floor_To_Exit_At = Person.Floor_At then
                     Number_Of_People_On_The_Correct_Floor := Number_Of_People_On_The_Correct_Floor + 1;
                  end if;
               end loop;
            end loop;
            return Number_Of_People_On_The_Correct_Floor;
         end;
      end How_Many_People_Are_On_The_Correct_Floor;

      function How_Many_People_Are_On_The_Wrong_Floor return Natural is
      begin
         declare
            Number_Of_People_On_The_Wrong_Floor : Natural := 0;
         begin
            for F in Floor_Range loop
               for Person of People_Outside_Elevator (F) loop
                  if Person.Floor_To_Exit_At /= Person.Floor_At then
                     Number_Of_People_On_The_Wrong_Floor := Number_Of_People_On_The_Wrong_Floor + 1;
                  end if;
               end loop;
            end loop;
            return Number_Of_People_On_The_Wrong_Floor;
         end;
      end How_Many_People_Are_On_The_Wrong_Floor;

      procedure Print_Elevator is
      begin
         Print (True, "People in Elevator:" & Elevator.How_Many_People_In_Elevator'Image);
         Print (True, "People outside Elevator correctly:" & Elevator.How_Many_People_Are_On_The_Correct_Floor'Image);
         Print (True, "People outside Elevator incorrectly:" & Elevator.How_Many_People_Are_On_The_Wrong_Floor'Image);
         if Ended then
            Print (True, "Total time taken in Time Units:" & Duration'Image ((End_Time - Start_Time) / Time_Unit));
            Print (True, "Time taken for elevator in Time Units:" & Used_Time_Units'Image);
         end if;
      end Print_Elevator;

      procedure Close_Elevator is
      begin
         Moving := True;
         End_Time := Clock;
         Ended := True;
      end Close_Elevator;
   end Elevator;

   procedure Add_People_To_Building (Manager : Building_Manager_Access; Seed : Integer := Integer'First) is
      subtype Floor_Range_Random is Floor_Range range Floor_Range'First .. Top_Floor;
      subtype People_On_Floor_Range_Random is Natural range 0 .. 10;
      package Random_Floor is new Ada.Numerics.Discrete_Random (Floor_Range_Random);
      package Random_People_On_Floor is new Ada.Numerics.Discrete_Random (People_On_Floor_Range_Random);
      Seed_People_On_Floor : Random_People_On_Floor.Generator;
      Random_Result_People_On_Floor  : People_On_Floor_Range_Random;
      Seed_Floors : Random_Floor.Generator;
   begin
      if Seed = Integer'First then
         Random_People_On_Floor.Reset (Seed_People_On_Floor);
         Random_Floor.Reset (Seed_Floors);
      else 
         Random_People_On_Floor.Reset (Seed_People_On_Floor, Seed);
         Random_Floor.Reset (Seed_Floors, Seed);
      end if;
      for Floor in Floor_Range'First .. Top_Floor loop
         Random_Result_People_On_Floor := Random_People_On_Floor.Random (Seed_People_On_Floor);
         for Person_Index in 1 .. Random_Result_People_On_Floor loop
            declare
               Random_Result_Floor_Enter_At : constant Floor_Range_Random := Random_Floor.Random (Seed_Floors);
               Random_Result_Floor_Exit_At : Floor_Range_Random := Random_Floor.Random (Seed_Floors);
            begin
               while Random_Result_Floor_Enter_At = Random_Result_Floor_Exit_At loop
                  Random_Result_Floor_Exit_At := Random_Floor.Random (Seed_Floors);
               end loop;
               declare
                  Person : constant People := (Random_Result_Floor_Exit_At, Random_Result_Floor_Enter_At, Random_Result_Floor_Enter_At);
               begin
                  Manager.Elevator.Add_Person (Person, Random_Result_Floor_Enter_At);
               end;
            end;
         end loop;
      end loop;
   end Add_People_To_Building;

   --  A Constructor. The variable (Building_Manager) you enter as a parameter when calling the constructor will be initialized.
   procedure Construct_Building_Manager (Manager : out Building_Manager_Access; Seed : Integer := Integer'First) is
      Elevator : constant Elevator_Access := new Building.Elevator;
      Manager1 : constant Building_Manager_Access := new Building_Manager (Elevator);
   begin
      Time_To_Enter := Time_Unit * 1;
      Time_To_Exit := Time_Unit * 1;
      Time_To_Move_A_Floor := Time_Unit * 3;
      Add_People_To_Building (Manager1, Seed);
      Manager := Manager1;
   end Construct_Building_Manager;

   task body Building_Manager is
      People_Mover1 : constant People_Mover_Access := new People_Mover (Elevator);
      procedure Move_Elevator_To_Floor (Floor_Going_To : Floor_Range) is
      begin
         Elevator.Move_Elevator_To_Floor (Floor_Going_To);
         Print (Is_Printing, "Arrived at floor:" & Floor_Going_To'Image);
         People_Mover1.Move_People (Floor_Going_To);
      end Move_Elevator_To_Floor;
   begin
      loop
         select
            accept Go_To_Floor (Floor_Going_To : Floor_Range) do
               Move_Elevator_To_Floor (Floor_Going_To);
            end Go_To_Floor;
         or
            accept Print_Elevator do
               null;
            end Print_Elevator;
            Elevator.Print_Elevator;
         or
            accept Close_Elevator do
               Elevator.Close_Elevator;
            end Close_Elevator;
         or
            terminate;
         end select;
      end loop;
   end Building_Manager;

   --  A task which lets people exit and enter the elevator.
   --  It doesn't block, as the delays are after, outside, the accept block.
   --  If done, it will exit, so it doesn't run forever, and it's to be entered after the elevator arrives at a floor.
   task body People_Mover is
      Current_Floor : Floor_Range;

      procedure Let_People_Exit_Elevator is
      begin
         Print (Is_Printing, "People exiting on floor:" & Current_Floor'Image);
         loop
            delay Time_To_Exit;
            Used_Time_Units := Used_Time_Units + 1;
            --  Ada has the ability to calculate one boolean before the other in an "or" or "and" statement.
            --  This is done with "or else" or "and else"
            if Elevator.Which_Floor_Is_Elevator_On /= Current_Floor or else Elevator.Is_Moving then
               Print (Is_Printing, "Not all people exited on floor:" & Current_Floor'Image);
               exit;
            end if;
            if Elevator.How_Many_People_Going_To_Floor_In_Elevator (Current_Floor) /= 0 then
               Elevator.Person_Leave_Elevator;
            else
               Print (Is_Printing, "All people exited on floor:" & Current_Floor'Image);
               exit;
            end if;
         end loop;
      end Let_People_Exit_Elevator;

      procedure Let_People_Enter_Elevator is
      begin
         Print (Is_Printing, "People entering on floor: " & Current_Floor'Image);
         loop
            delay Time_To_Enter;
            Used_Time_Units := Used_Time_Units + 1;
            if Elevator.Which_Floor_Is_Elevator_On /= Current_Floor or else Elevator.Is_Moving then
               Print (Is_Printing, "Not all people entered on floor:" & Current_Floor'Image);
               exit;
            end if;
            if Elevator.How_Many_People_Waiting_On_Floor (Current_Floor) /= 0 then
               Elevator.Person_Enter_Elevator;
            else
               Print (Is_Printing, "All people entered on floor:" & Current_Floor'Image);
               exit;
            end if;
         end loop;
      end Let_People_Enter_Elevator;

   begin
      loop
         select
               --  This entry will set the current floor and then, without blocking, handle the elevator.
               accept Move_People (Floor : Floor_Range) do
                  Current_Floor := Floor;
               end Move_People;
               Let_People_Exit_Elevator;
               Let_People_Enter_Elevator;
            or
               terminate;
         end select;
      end loop;
   end People_Mover;

   procedure Do_Not_Print is
   begin
      Is_Printing := False;
   end Do_Not_Print;

   procedure Do_Print is 
   begin
      Is_Printing := True;
   end Do_Print;
end Building;