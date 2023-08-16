with Building; use Building;
procedure ElevatorExample is
   Manager : Building_Manager_Access;
begin
   Construct_Building_Manager (Manager);
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
   Manager.Close_Elevator;
   Manager.Print_Elevator;
end ElevatorExample;
