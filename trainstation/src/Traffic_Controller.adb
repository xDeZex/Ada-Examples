with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

with Ada.Calendar.Formatting;
use  Ada.Calendar.Formatting;

package body Traffic_Controller is

   task body Add_Train is
      package Train_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
      Element_Type => Train);
      Trains : Train_Vectors.Vector;
      Controller1 : Controller_Access;
   begin
      loop 
         select
            accept Add_Train (aTrain : Train) do
               Trains.Append (aTrain);
            end Add_Train;
         or
            accept Start (aController : Controller_Access) do
               Controller1 := aController;
            end Start;
            for aTrain of Trains loop
               Controller1.New_Train (aTrain); 
               delay 0.5;
            end loop;
         or 
            terminate;
         end select;
         
      end loop;
      
   end Add_Train;

   task body Controller is
      aTrain_Task : Train_Task_Access;
      Trains_To_Be_Deleted : Train_Vectors.Vector;
      procedure Assign_Train (aTrack : Track_Access; aTrain : Train) is
      begin
         if aTrack = null then
            Put_Line ("Failed to assign: " & aTrain.ID'Image);
            return;
         end if;
         aTrack.Assign_Train (aTrain.ID, aTrain.Size, aTrain.Clear_Time);
         aTrain_Task := new Train_Task (aTrain.ID, aTrain.Size, aTrain.Clear_Time, aTrack);
         Trains_To_Be_Deleted.Append (aTrain);
      end Assign_Train;
      Number_Of_Added_Trains : Integer := 0;

      function Chosen_Track (aTrain : Train) return Track_Access is
         Candidate_Tracks : Track_Vectors.Vector;
         Time_Done : Time := Duration (aTrain.Clear_Time) / 1000.0 + Clock;
         Best_Track : Track_Access;
      begin
         for aTrack of Tracks loop
            if aTrack.Space_Left >= aTrain.Size then
               Candidate_Tracks.Append (aTrack);
            end if;
         end loop;
         if Integer (Candidate_Tracks.Length) = 0 then
            return null;
         end if;
         declare 
            Best_Score : Natural := 0;
            Scores : array (0 .. Integer (Candidate_Tracks.Length) - 1) of Natural;
            I : Natural := Natural'First;
         begin
            for aTrack of Candidate_Tracks loop
               if aTrack.Space_Left >= aTrain.Size then
                  Scores (I) := aTrack.Space_Left;
                  if aTrack.When_Last_Clear_Time < Time_Done then
                     Scores (I) := Scores (I) + 3;
                  end if;
                  if Clock + (aTrack.When_Last_Clear_Time - Clock) / 10.0 < Time_Done and Clock - (aTrack.When_Last_Clear_Time - Clock) / 10.0 > Time_Done then
                     Scores (I) := Scores (I) + 2;
                  end if;
                  if aTrack.Space_Left - aTrain.Size > aTrain.Size then
                     Scores (I) := Scores (I) + 2;
                  end if;
                  if aTrack.Space_Left - aTrain.Size <= aTrain.Size then
                     if aTrack.When_Last_Clear_Time > Time_Done then
                        if Scores (I) < 2 then
                           Scores (I) := 0;
                        else
                           Scores (I) := Scores (I) - 2;
                        end if;
                     end if;
                  end if;
                  I := I + 1;
               end if;
            end loop;
            for Index_Score in 0 .. Integer (Candidate_Tracks.Length) - 1 loop
               if (Scores (Index_Score) > Best_Score) then
                  Best_Score := Scores (Index_Score);
                  Best_Track := Candidate_Tracks (Index_Score);
               end if;
            end loop;

            return Best_Track;
         end;

      end Chosen_Track;
   begin
      loop
         select
            accept Add_Track (T : Track_Access) do
               Tracks.Append (T);
            end Add_Track;
         or
            accept New_Train (T : Train) do
               Trains.Append (T);
               Number_Of_Added_Trains := Number_Of_Added_Trains + 1;
            end New_Train;
         or
            accept Cleared_Train do
               null;
            end Cleared_Train;
         or
            accept Check_Trains do
               --Put_Line (Trains'Image);
               for aTrain of Trains loop
                  Assign_Train (Chosen_Track (aTrain), aTrain);
               end loop;
               for aTrain of Trains_To_Be_Deleted loop
                  Trains.Delete (Trains.Find_Index (aTrain));
               end loop;
               Trains_To_Be_Deleted.Clear;
            end Check_Trains;
         or
            accept Show_Tracks do
               for aTrack of Tracks loop
                  aTrack.Show_Track;
               end loop;
            end Show_Tracks;
         or
            accept Waiting_Trains do
               Put_Line ("Unassigned: " & Trains.Length'Image);
            end Waiting_Trains;
         or 
            accept Is_Done (Is_Done : out Boolean) do
               Is_Done := Trains.Is_Empty and Number_Of_Added_Trains = Number_Of_Trains;
               if Is_Done then
                  for aTrack of Tracks loop
                     Is_Done := aTrack.Is_Done;
                     if not Is_Done then
                        exit;
                     end if;
                  end loop;
               end if;
            end Is_Done;
         or
            terminate;
         end select;
      end loop;
   end Controller;

   task body Train_Task is
   begin
      --Put_Line (Clear_Time'Image & " " & ID'Image);
      delay Duration (Clear_Time) / 1000.0;
      --Put_Line ("Done: " & ID'Image & " " & Clear_Time'Image);
      My_Track.Clear_Train (ID, Size);
   end Train_Task;

   protected body Track is
      procedure Show_Track is
      begin
         Put_Line (ID'Image & ": " & Trains'Image & " " & Image (Last_Clear_Time, True) & " Now: " & Image (Clock, True));
      end Show_Track;
      entry Assign_Train (Train_ID : Integer; Train_Size : Integer; Clear_Time : Integer) when Size /= Filled is
      begin
         Trains.Append (Train_ID);
         Train_Sizes.Append (Train_Size);
         Filled := Filled + Train_Size;
         declare
            temp : Time := Clock + Duration (Clear_Time) / 1000.0;
         begin
            Put_Line ("Assigned ID: " & Train_ID'Image & " " & Image (temp, True) & " -- To Track: " & ID'Image);
         end;
         if Clock + Duration (Clear_Time) / 1000.0 > Last_Clear_Time then
            Last_Clear_Time := Clock + Duration (Clear_Time) / 1000.0;
         end if;
      end Assign_Train;

      entry Clear_Train (Train_ID : Integer; Train_Size : Integer) when True is
      begin
         --Put_Line ("Cleared: " & Train_ID'Image & " from ID: " & ID'Image);
         Trains_Cleared.Append (Train_ID);
         declare
            i : Integer := 0;
         begin
            for aTrain of Trains loop
               --Put_Line ("left: " & aTrain'Image);
               if not Trains_Cleared.Contains (aTrain) then
                  exit;
               end if;
               i := i + 1;
            end loop;
            --Put_Line ("Clearing: " & i'Image & " on track: " & ID'Image);
            if i > 0 then
               for j in 1 .. i loop
                  --Put_Line (i'Image & " " & Trains'Image);
                  Filled := Filled - Train_Sizes (0);
                  Trains.Delete_First;
                  Train_Sizes.Delete_First;
               end loop;
            end if;
            if Trains.Is_Empty then
               Last_Clear_Time := Clock;
            end if;
            --Put_Line ("After clearing: " & Trains'Image);
         end;
      end Clear_Train;

      function Space_Left return Integer is
      begin
         return Size - Filled;
      end Space_Left;

      function Is_Done return Boolean is
      begin
         return Filled = 0;
      end Is_Done;

      function When_Last_Clear_Time return Time is
      begin
         return Last_Clear_Time;
      end When_Last_Clear_Time;
   end Track;
end Traffic_Controller;