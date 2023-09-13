with Building; use Building;
with Ada.Text_IO; use Ada.Text_IO;
procedure ElevatorExample is
   Manager : Building_Manager_Access;
begin
   --  Creates a manager and starts the timer
   Construct_Building_Manager (Manager, Seed => 1);

   --  Choose to print information about the elevator or not, can be changed any time.
   --Do_Print;
   --  Do_Not_Print;
   for Floor in Floor_Range'First .. Top_Floor loop
      Manager.Go_To_Floor (Floor);
      delay Time_To_Exit * 10;
      delay Time_To_Enter * 10;
   end loop;
   for Floor in reverse Floor_Range'First .. Top_Floor - 1 loop
      Manager.Go_To_Floor (Floor);
      delay Time_To_Exit * 10;
      delay Time_To_Enter * 10;
   end loop;

   --  Stops the timer and prints the results
   Manager.Close_Elevator;
   Manager.Print_Elevator;
end ElevatorExample;