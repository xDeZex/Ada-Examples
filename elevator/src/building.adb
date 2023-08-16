with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
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
      entry Move_Elevator_To_Floor (Floor_Going_To : Floor_Range) when not Moving is
      begin
         Put_Line ("Started Moving");
         if not Started then
            Started := True;
            Start_Time := Clock;
         end if; 
         Moving := True;
         delay abs (Floor - Floor_Going_To) * Time_To_Move_A_Floor;
         Floor := Floor_Going_To;
         Moving := False;
      end Move_Elevator_To_Floor;

      function How_Many_People_Going_To_Floor (Floor_In_Question : Floor_Range) return Natural is
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
      end How_Many_People_Going_To_Floor;

      function How_Many_People_Waiting_On_Floor (Floor_In_Question : Floor_Range) return Natural is
      begin
         declare
            Number_Of_People_Waiting_On_Floor : Natural := 0;
         begin
            for Person of People_Outside_Elevator loop
               if Person.Floor_To_Enter_At = Floor_In_Question and then Person.Floor_At = Floor_In_Question then
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

      procedure Add_Person (Person : People) is
      begin
         People_Outside_Elevator.Append (Person);
      end Add_Person;

      procedure Person_Leave_Elevator is
      begin
         if not People_In_Elevator.Is_Empty then
            for i in 0 .. Integer (People_In_Elevator.Length) loop
               if People_In_Elevator (i).Floor_To_Exit_At = Floor then
                  declare
                     temp : People := People_In_Elevator (i);
                  begin
                     temp.Floor_At := Floor;
                     People_Outside_Elevator.Append (temp);
                     People_In_Elevator.Delete (i);
                     Put_Line ("Person exited to floor:" & Floor'Image);
                     exit;
                  end;
               end if;
            end loop;
         end if;
      end Person_Leave_Elevator;

      procedure Person_Enter_Elevator  is
      begin
         if not People_Outside_Elevator.Is_Empty then
            for i in 0 .. Integer (People_Outside_Elevator.Length) loop
               if People_Outside_Elevator (i).Floor_To_Enter_At = Floor then
                  People_In_Elevator.Append (People_Outside_Elevator (i));
                  People_Outside_Elevator.Delete (i);
                  Put_Line ("Person entered from floor:" & Floor'Image);
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
            for Person of People_Outside_Elevator loop
               if Person.Floor_To_Exit_At = Person.Floor_At then
                  Number_Of_People_On_The_Correct_Floor := Number_Of_People_On_The_Correct_Floor + 1;
               end if;
            end loop;
            return Number_Of_People_On_The_Correct_Floor;
         end;
      end How_Many_People_Are_On_The_Correct_Floor;

      function How_Many_People_Are_On_The_Wrong_Floor return Natural is
      begin
         declare
            Number_Of_People_On_The_Wrong_Floor : Natural := 0;
         begin
            for Person of People_Outside_Elevator loop
               if Person.Floor_To_Exit_At /= Person.Floor_At then
                  Number_Of_People_On_The_Wrong_Floor := Number_Of_People_On_The_Wrong_Floor + 1;
               end if;
            end loop;
            return Number_Of_People_On_The_Wrong_Floor;
         end;
      end How_Many_People_Are_On_The_Wrong_Floor;

      procedure Print_Elevator is
      begin
         Put_Line ("People in Elevator:" & Elevator.How_Many_People_In_Elevator'Image);
         Put_Line ("People outside Elevator correctly:" & Elevator.How_Many_People_Are_On_The_Correct_Floor'Image);
         Put_Line ("People outside Elevator incorrectly:" & Elevator.How_Many_People_Are_On_The_Wrong_Floor'Image);
         if Ended then
            Put_Line ("Time taken in Time Units:" & Duration'Image ((End_Time - Start_Time) / Time_Unit));
         end if;
      end Print_Elevator;

      procedure Close_Elevator is
      begin
         Moving := True;
         End_Time := Clock;
         Ended := True;
      end Close_Elevator;

   end Elevator;

   procedure Add_People_To_Building (Manager : Building_Manager_Access) is
      subtype Floor_Range_Random is Floor_Range range Floor_Range'First .. Top_Floor;
      subtype People_On_Floor_Range_Random is Natural range 0 .. 10;
      package Random_Floor is new Ada.Numerics.Discrete_Random (Floor_Range_Random);
      package Random_People_On_Floor is new Ada.Numerics.Discrete_Random (People_On_Floor_Range_Random);
      Seed_People_On_Floor : Random_People_On_Floor.Generator;
      Random_Result_People_On_Floor  : People_On_Floor_Range_Random;
      Seed_Floors : Random_Floor.Generator;
      Random_Result_Floors  : Floor_Range_Random;
   begin
      Random_People_On_Floor.Reset (Seed_People_On_Floor);
      Random_Floor.Reset (Seed_Floors, 1);
      for Floor in Floor_Range'First .. Top_Floor loop
         Random_Result_People_On_Floor := Random_People_On_Floor.Random (Seed_People_On_Floor);
         for Person_Index in 1 .. Random_Result_People_On_Floor loop
            declare
               Random_Result_Floor_Enter_At : Floor_Range_Random := Random_Floor.Random (Seed_Floors);
               Random_Result_Floor_Exit_At : Floor_Range_Random := Random_Floor.Random (Seed_Floors);
            begin
               while Random_Result_Floor_Enter_At = Random_Result_Floor_Exit_At loop
                  Random_Result_Floor_Exit_At := Random_Floor.Random (Seed_Floors);
               end loop;
               declare 
                  Person : People := (Random_Result_Floor_Exit_At, Random_Result_Floor_Enter_At, Random_Result_Floor_Enter_At);
               begin
                  Manager.Elevator1.Add_Person (Person);
               end;
            end;
         end loop;
      end loop;
   end Add_People_To_Building;

   --  A Constructor. The variable you enter as a parameter when calling the constructor will be initialized.
   procedure Construct_Building_Manager (Manager : out Building_Manager_Access) is
      Elevator1 : constant Elevator_Access := new Elevator;
      Manager1 : constant Building_Manager_Access := new Building_Manager (1, Elevator1);
   begin
      Add_People_To_Building (Manager1);
      Manager := Manager1;
   end Construct_Building_Manager;

   task body Building_Manager is
      People_Mover1 : constant People_Mover_Access := new People_Mover (Elevator1);
      procedure Move_Elevator_To_Floor (Floor_Going_To : Floor_Range) is
      begin
         Elevator1.Move_Elevator_To_Floor (Floor_Going_To);
         Put_Line ("Arrived at floor:" & Floor_Going_To'Image);
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
            Elevator1.Print_Elevator;
         or
            accept Close_Elevator do
               Elevator1.Close_Elevator;
            end Close_Elevator;
         or
            terminate;
         end select;
      end loop;
   end Building_Manager;

   task body People_Mover is
      Current_Floor : Floor_Range;

      procedure Let_People_Exit_Elevator is
      begin
         Put_Line ("People exiting on floor:" & Current_Floor'Image);
         loop
            if Elevator1.Which_Floor_Is_Elevator_On /= Current_Floor or else Elevator1.Is_Moving then
               Put_Line ("Not all people exited on floor:" & Current_Floor'Image);
               exit;
            end if;
            if Elevator1.How_Many_People_Going_To_Floor (Current_Floor) /= 0 then
               Elevator1.Person_Leave_Elevator;
            else
               Put_Line ("All people exited on floor:" & Current_Floor'Image);
               exit;
            end if;
            delay Time_To_Exit;
         end loop;
      end Let_People_Exit_Elevator;

      procedure Let_People_Enter_Elevator is
      begin
         Put_Line ("People entering on floor: " & Current_Floor'Image);
         loop
            if Elevator1.Which_Floor_Is_Elevator_On /= Current_Floor or else Elevator1.Is_Moving then
               Put_Line ("Not all people entered on floor:" & Current_Floor'Image);
               exit;
            end if;
            if Elevator1.How_Many_People_Waiting_On_Floor (Current_Floor) /= 0 then
               Elevator1.Person_Enter_Elevator;
            else
               Put_Line ("All people entered on floor:" & Current_Floor'Image);
               exit;
            end if;
            delay Time_To_Enter;
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
end Building;