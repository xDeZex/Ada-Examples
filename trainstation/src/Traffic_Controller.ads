with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;
package Traffic_Controller is

   package Integer_Vectors is new Ada.Containers.Vectors
   (Index_Type   => Natural,
   Element_Type => Natural);

   

   protected type Track (ID : Integer) is
      entry Assign_Train (Train_ID : Integer; Train_Size : Integer; Clear_Time : Integer);
      entry Clear_Train (Train_ID : Integer; Train_Size : Integer);
      function Space_Left return Integer;
      procedure Show_Track;
      function Is_Done return Boolean;
      function When_Last_Clear_Time return Time;
      --  entry Wait_For_Train;
   private
      Last_Clear_Time : Time := Clock;
      Trains : Integer_Vectors.Vector;
      Train_Sizes : Integer_Vectors.Vector;
      Trains_Cleared : Integer_Vectors.Vector;
      Size : Natural := 8;
      Filled : Natural := 0;
   end Track;
   type Track_Access is access all Track;

   type Train is record
      ID : Integer;
      Size : Integer;
      Clear_Time : Integer;
   end record;
   type Train_Access is access all Train;

   task type Train_Task (ID : Integer; Size : Integer; Clear_Time : Integer; My_Track : Track_Access) is
   end Train_Task;
   type Train_Task_Access is access all Train_Task;

   task type Controller (Number_Of_Trains : Integer) is
      entry New_Train (T : Train);
      entry Cleared_Train;
      entry Add_Track (T : Track_Access);
      entry Check_Trains;
      entry Show_Tracks;
      entry Is_Done (Is_Done : out Boolean);
      entry Waiting_Trains;
   end Controller;
   type Controller_Access is access all Controller;

    task type Add_Train is
      entry Add_Train (aTrain : Train);
      entry Start (aController : Controller_Access);
   private
   end Add_Train;
   type Add_Train_Access is access all Add_Train;

   
   type Train_Array is array (1 .. 20) of Train;
   Trains_New : Train_Array;

private
   package Track_Vectors is new Ada.Containers.Vectors
   (Index_Type   => Natural,
   Element_Type => Track_Access);
   Tracks : Track_Vectors.Vector;
   package Train_Vectors is new Ada.Containers.Vectors
   (Index_Type   => Natural,
   Element_Type => Train);
   Trains : Train_Vectors.Vector;
end Traffic_Controller;