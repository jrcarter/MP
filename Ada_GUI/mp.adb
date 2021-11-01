-- Ada-GUI version of MP: a Music Player
-- Copyright (C) 2021 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Ada_GUI;

with PragmARC.Persistent_Skip_List_Unbounded;

procedure MP is
   package B_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 400);
   use B_Strings;

   subtype Path_Name is Bounded_String;

   function Less (Left : Path_Name; Right : Path_Name) return Boolean;
   -- Files in the working directory are < files in subdirectories

   package Path_Lists is new PragmARC.Persistent_Skip_List_Unbounded (Element => Path_Name, "<" => Less);

   type Song_Info is record
      Position : Positive;
      Path     : Path_Name;
   end record;

   package Song_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Song_Info);

   function Less (Left : Path_Name; Right : Path_Name) return Boolean is
      function Slash_Count (Path : Path_Name) return Natural;

      function Slash_Count (Path : Path_Name) return Natural is
         Result : Natural := 0;

         Path_S : constant String := To_String (Path);
      begin -- Slash_Count
         All_Chars : for I in Path_S'Range loop
            if Path_S (I) = '/' then
               Result := Result + 1;
            end if;
         end loop All_Chars;

         return Result;
      end Slash_Count;

      Count_L : constant Natural := Slash_Count (Left);
      Count_R : constant Natural := Slash_Count (Right);
   begin -- Less
      if Count_L = 0 and Count_R > 0 then
         return True;
      end if;

      if Count_R = 0 and Count_L > 0 then
         return False;
      end if;

      return Left < Right;
   end Less;

   List : Path_Lists.Persistent_Skip_List := Path_Lists.Open_List ("playlist.mpl");
   Song : Song_Lists.Vector;

   procedure Make_Song_List (List : in out Song_Lists.Vector);
   -- Adds all the songs in MP.List to List

   procedure Shuffle (List : in out Song_Lists.Vector);
   -- Randomizes List

   procedure Add_Song;
   -- Adds Path to List if Path is not null

   procedure Browse_Songs;
   -- Displays a file-selection dialog and allows selection of a file in the current directory or below
   -- If a file is selected, puts its name relative to the current directory in Path

   procedure Delete_Song;
   -- Deletes the currently selected song

   procedure Quit_Now;
   -- Tears down the GUI and saves the playlist

   procedure Refresh;
   -- Shuffles List into Song and updates Count

   function Start (Song : in String) return Boolean;
   -- Tries to start playing a file named Song
   -- Returns True if successful; False otherwise

   Player     : Ada_GUI.Widget_ID;
   Sel        : Ada_GUI.Widget_ID;
   Count      : Ada_GUI.Widget_ID;
   Delete     : Ada_GUI.Widget_ID;
   Path       : Ada_GUI.Widget_ID;
   Browse     : Ada_GUI.Widget_ID;
   Add        : Ada_GUI.Widget_ID;
   Play       : Ada_GUI.Widget_ID;
   Skip       : Ada_GUI.Widget_ID;
   Quit       : Ada_GUI.Widget_ID;
   Quit_After : Ada_GUI.Widget_ID;
   Event      : Ada_GUI.Next_Result_Info;

   Current : Song_Info;
   Index   : Positive := 1;

   procedure Make_Song_List (List : in out Song_Lists.Vector) is
      Position : Natural := 0;

      procedure Add_One (Item : in Path_Name) is
         -- Empty;
      begin -- Add_One
         Position := Position + 1;
         List.Append (New_Item => (Position => Position, Path => Item) );
         Sel.Insert (Text => To_String (Item) );
      end Add_One;

      procedure Add_All is new Path_Lists.Iterate (Action => Add_One);
   begin -- Make_Song_List
      List.Clear;
      Add_All (List => MP.List);
   end Make_Song_List;

   procedure Shuffle (List : in out Song_Lists.Vector) is
      subtype Index is Integer range 1 .. List.Last_Index;

      package Index_Random is new Ada.Numerics.Discrete_Random (Result_Subtype => Index);

      Gen : Index_Random.Generator;
      J   : Index;
      T   : Song_Info;
   begin -- Shuffle
      Index_Random.Reset (Gen => Gen);

      Swap_All : for I in 1 .. List.Last_Index loop
         J := Index_Random.Random (Gen);
         T := List.Element (I);
         List.Replace_Element (Index => I, New_Item => List.Element (J) );
         List.Replace_Element (Index => J, New_Item => T);
      end loop Swap_All;
   end Shuffle;

   procedure Add_Song is
      Name : constant String := Path.Text;
   begin -- Add_Song
      if Name = "" then
         return;
      end if;

      if not Player.Playback_Ended then
         Player.Pause;
      end if;

      List.Insert (Item => To_Bounded_String (Name) );
      Refresh;
      Path.Set_Text (Text => "");

      if Player.Paused then
         Player.Play;
      end if;
   exception -- Add_Song
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Add_Song " & Ada.Exceptions.Exception_Information (Error) );
   end Add_Song;

   Current_Directory : constant String := Ada.Directories.Current_Directory;

   procedure Browse_Songs is
      Pick : Ada_GUI.File_Result_Info := Ada_GUI.Selected_File (Current_Directory);
   begin -- Browse_Songs
      if not Pick.Picked then
         return;
      end if;

      Get_Name : declare
         Name : constant String := Ada.Strings.Unbounded.To_String (Pick.Value);
      begin -- Get_Name
         if Name'Length > Current_Directory'Length and then
            Name (Name'First .. Name'First + Current_Directory'Length - 1) = Current_Directory
         then
            Path.Set_Text (Text => Name (Name'First + Current_Directory'Length + 1 .. Name'Last) );
         end if;
      end Get_Name;
   exception -- Browse_Songs
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Browse_Songs " & Ada.Exceptions.Exception_Information (Error) );
   end Browse_Songs;

   procedure Delete_Song is
      Index : constant Natural := Sel.Selected;
   begin -- Delete_Song
      if Index = 0 then
         return;
      end if;

      if not Player.Playback_Ended then
         Player.Pause;
      end if;

      List.Delete (Item => To_Bounded_String (Sel.Text (Index) ) );
      Refresh;

      if Player.Paused then
         Player.Play;
      end if;
   exception -- Delete_Song
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Delete_Song " & Ada.Exceptions.Exception_Information (Error) );
   end Delete_Song;

   Title : constant String := "MP";

   procedure Quit_Now is
      -- Empty
   begin -- Quit_Now
      Ada_GUI.Set_Title (Title => Title);
      Ada_GUI.End_GUI;
   exception -- Quit_Now
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Quit_Now " & Ada.Exceptions.Exception_Information (Error) );
   end Quit_Now;

   procedure Refresh is
      -- Empty
   begin -- Refresh
      Sel.Clear;
      Make_Song_List (List => Song);
      Count.Set_Text (Text => Integer'Image (Sel.Length) );
      Shuffle (List => Song);
   exception -- Refresh
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Refresh " & Ada.Exceptions.Exception_Information (Error) );
   end Refresh;

   function Start (Song : in String) return Boolean is
      -- Empty
   begin -- Start
   Ada.Text_IO.Put_Line("Start "&Song);
      Player.Set_Source (Source => Song);
      Ada_GUI.Set_Title (Title => Title & ' ' & Song);

      Wait_For_Ready : for I in 1 .. 10 loop
         if Player.Ready then
            Player.Play;
   Ada.Text_IO.Put_Line("Start started "&Song);

            return True;
         end if;

         delay 0.01;
      end loop Wait_For_Ready;
   Ada.Text_IO.Put_Line("Start failed "&Song);

      return False;
   exception -- Start
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Start " & Ada.Exceptions.Exception_Information (Error) );

      return False;
   end Start;

   use type Ada_GUI.Event_Kind_ID;
   use type Ada_GUI.Widget_ID;
begin -- MP
   Ada_GUI.Set_Up (Title => Title, ID => 8089);
   Player     := Ada_GUI.New_Audio_Player;
   Sel        := Ada_GUI.New_Selection_List (Break_Before => True, Height => 30);
   Count      := Ada_GUI.New_Text_Box (Label => "Number of songs:", Break_Before => True);
   Delete     := Ada_GUI.New_Button (Text => "Delete", Break_Before => True);
   Path       := Ada_GUI.New_Text_Box (Width => 100);
   Browse     := Ada_GUI.New_Button (Text => "Browse");
   Add        := Ada_GUI.New_Button (Text => "Add");
   Play       := Ada_GUI.New_Button (Text => "Play", Break_Before => True);
   Skip       := Ada_GUI.New_Button (Text => "Skip");
   Quit       := Ada_GUI.New_Button (Text => "Quit");
   Quit_After := Ada_GUI.New_Check_Box (Label => "Quit after this song");

   Refresh;

   -- Now we're ready to go

   -- If Song is empty, we wait until a song is added or the user clicks on Quit, ignoring other buttons
   Wait_For_Song : loop
      exit Wait_For_Song when List.Length > 0;

      Event := Ada_GUI.Next_Event;

      if not Event.Timed_Out and then Event.Event.Kind = Ada_GUI.Left_Click then
         if Event.Event.ID = Add then
            Add_Song;
         elsif Event.Event.ID = Browse then
            Browse_Songs;
         elsif Event.Event.ID = Quit then
            Quit_Now;

            return;
         else
            null;
         end if;
      end if;
   end loop Wait_For_Song;

   Forever : loop
      Current := Song.Element (Index);
      Sel.Set_Selected (Index => Current.Position);

      if Start (Sel.Text) then
         Wait_For_End : loop
            Event := Ada_GUI.Next_Event (Timeout => 0.1);

            if Event.Timed_Out then
               exit Wait_For_End when Player.Playback_Ended;
            elsif Event.Event.Kind = Ada_GUI.Left_Click then
               if Event.Event.ID = Play then
                  exit Wait_For_End when not Start (Sel.Text);
               elsif Event.Event.ID = Skip then
                  Ada_GUI.Set_Title (Title => Title);

                  exit Wait_For_End;
               elsif Event.Event.ID = Quit then
                  Quit_Now;

                  return;
               elsif Event.Event.ID = Add then
                  Add_Song;
               elsif Event.Event.ID = Browse then
                  Browse_Songs;
               elsif Event.Event.ID = Delete then
                  Delete_Song;
               else
                  null;
               end if;
            else
               null;
            end if;
         end loop Wait_For_End;
      end if;

      exit Forever when Quit_After.Active;

      Index := Index + 1;

      if Index > Song.Last_Index then
         Index := 1;
      end if;
   end loop Forever;

   Quit_Now;
exception -- MP
when others =>
   if Ada_GUI.Set_Up then
      Quit_Now;
   end if;
end MP;
