with Ada.Numerics.Discrete_Random;
with Traffic_Controller; use Traffic_Controller;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Calendar;

with Ada.Calendar.Formatting;
use  Ada.Calendar.Formatting;

procedure Train_Station is
   New_Track1 : Track_Access := new Track (1);
   New_Track2 : Track_Access := new Track (2);
   New_Track3 : Track_Access := new Track (3);

   subtype Custom is Integer range 2000 .. 8000;
   package Rand_Custom is new Ada.Numerics.Discrete_Random (Custom);
   use Rand_Custom;
   Seed : Generator;
   Num  : Custom;

   subtype Train_IDs is Integer range 1 .. 2;

   Controller1 : Controller_Access := new Controller (20);

   i : Natural := 0;

   Is_Done : Boolean := False;

   Start_Time : Time := Clock;
   End_Time : Time := Clock;

   Train_Adder : Add_Train_Access := new Add_Train;

   task Display_Tracks is
      entry Start;
   end Display_Tracks;
   task body Display_Tracks is
      
   begin
      select
         accept Start do
            null;
         end Start;
         loop 
            delay 0.5;
            if Is_Done then
               declare 
                  Time_Taken : Duration := End_Time - Start_Time;
               begin
                  Put_Line ("Done at time: " & Image (End_Time, True));
                  Put_Line ("Time taken: " & Time_Taken'Image);
                  Put_Line ("Time taken: " & Duration'Image (End_Time - Start_Time));
               end;
               exit;
            end if;
            Put_Line ("--------------------------------");
            Controller1.Show_Tracks;
            Controller1.Waiting_Trains;
         end loop;
      end select;
   end;
begin

   Controller1.Add_Track (New_Track1);
   Controller1.Add_Track (New_Track2);
   --Controller1.Add_Track (New_Track3);
   Reset (Seed, 1);
   for aTrain of Trains_New loop
      Num := Random (Seed);
      --Put_Line (aTrain'Image);
      aTrain.ID := i;
      aTrain.Size := 2;
      aTrain.Clear_Time := Num;
      Train_Adder.Add_Train (aTrain);
      i := i + 1;
   end loop;
   Train_Adder.Start (Controller1);
   --Put_Line (Trains_New'Image);
   declare
      Next : Time := Clock + 0.25;
      Now_Year       : Ada.Calendar.Year_Number;
      Now_Month      : Ada.Calendar.Month_Number;
      Now_Day        : Ada.Calendar.Day_Number;
      Now_Hour       : Hour_Number;
      Now_Minute     : Minute_Number;
      Now_Second     : Second_Number;
      Now_Sub_Second : Second_Duration;

   begin
      Split (Next, Now_Year, Now_Month, Now_Day, Now_Hour, Now_Minute, Now_Second, Now_Sub_Second);
      Next := Time_Of (Now_Year, Now_Month, Now_Day, Now_Hour, Now_Minute, Now_Second);
      Next := Next + Duration (1);
      Start_Time := Next;
      delay until Next;
      Display_Tracks.Start;
      loop
         Next := Next + 0.25;
         Controller1.Check_Trains;
         Controller1.Is_Done (Is_Done);
         if Is_Done then
            End_Time := Clock;
            exit;
         end if;
         delay until Next;
      end loop;
   end;
end Train_Station;
