-- Ada-GUI version of MP: a Music Player
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
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

   List : Path_Lists.Persistent_Skip_List := Path_Lists.Open_List ("playlist.mpl", Write_On_Modify => True);
   Song : Song_Lists.Vector;

   task Sel_Loader is
      entry Fill;
      -- Clears Sel, then adds all the songs in List to Sel
   end Sel_Loader;

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
   Load_Label : Ada_GUI.Widget_ID;
   Loading    : Ada_GUI.Widget_ID;
   Load_Count : Ada_GUI.Widget_ID;
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

   Current       : Song_Info;
   Index         : Positive := 1;
   Sel_Available : Boolean  := False with Atomic;

   task body Sel_Loader is
      Max    : Natural;
      Length : Natural;
      Count  : Natural := 0;

      procedure Add_One (Item : in Path_Name) is
         -- Empty;
      begin -- Add_One
         Sel.Insert (Text => To_String (Item) );
         Count := Count + 1;

         if Count rem 10 = 0 then
            Loading.Set_Value (Value => Max * Count / Length);
            Load_Count.Set_Text (Text => Count'Image);
         end if;
      end Add_One;

      procedure Add_All is new Path_Lists.Iterate (Action => Add_One);
   begin -- Sel_Loader
      Forever : loop
         select
            accept Fill;

            Max := Loading.Maximum;
            Length := List.Length;
            Sel_Available := False;
            Sel.Set_Visibility (Visible => False);
            Load_Label.Set_Visibility (Visible => True);
            Loading.Set_Visibility (Visible => True);
            Load_Count.Set_Visibility (Visible => True);
            Sel.Clear;
            Add_All (List => List);
            Sel.Set_Visibility (Visible => True);
            Sel_Available := True;
            Load_Label.Set_Visibility (Visible => False);
            Loading.Set_Visibility (Visible => False);
            Load_Count.Set_Visibility (Visible => False);
         or
            terminate;
         end select;
      end loop Forever;
   exception -- Sel_Loader
   when E : others =>
      Ada_GUI.Log (Message => "Sel_Loader " & Ada.Exceptions.Exception_Information (E) );
   end Sel_Loader;

   procedure Make_Song_List (List : in out Song_Lists.Vector) is
      Position : Natural := 0;

      procedure Add_One (Item : in Path_Name) is
         -- Empty;
      begin -- Add_One
         Position := Position + 1;
         List.Append (New_Item => (Position => Position, Path => Item) );
      end Add_One;

      procedure Add_All is new Path_Lists.Iterate (Action => Add_One);
   begin -- Make_Song_List
      Sel_Loader.Fill;
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

      List.Insert (Item => To_Bounded_String (Name) );
      Refresh;
      Path.Set_Text (Text => "");
   exception -- Add_Song
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Add_Song " & Ada.Exceptions.Exception_Information (Error) );
   end Add_Song;

   Current_Directory : constant String := Ada.Directories.Current_Directory;

   procedure Browse_Songs is
      Pick : Ada_GUI.Dialogs.File_Result_Info := Ada_GUI.Dialogs.Selected_File (Current_Directory);
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

      List.Delete (Item => To_Bounded_String (Sel.Text) );
      Refresh;
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
      Make_Song_List (List => Song);
      Count.Set_Text (Text => Integer'Image (List.Length) );
      Shuffle (List => Song);
   exception -- Refresh
   when Error : others =>
      Ada.Text_IO.Put_Line (Item => "Refresh " & Ada.Exceptions.Exception_Information (Error) );
   end Refresh;

   function Start (Song : in String) return Boolean is
      -- Empty
   begin -- Start
      Player.Set_Source (Source => Song);
      Ada_GUI.Set_Title (Title => Title & ' ' & Song);

      Wait_For_Ready : for I in 1 .. 10 loop
         if Player.Ready then
            Player.Play;

            return True;
         end if;

         delay 0.01;
      end loop Wait_For_Ready;

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
   Load_Label := Ada_GUI.New_Background_Text (Text => "Loading ", Break_Before => True);
   Loading    := Ada_GUI.New_Progress_Bar;
   Load_Count := Ada_GUI.New_Background_Text;
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

   All_Cycles : loop
      -- If Song is empty, we wait until a song is added or the user clicks on Quit or closes the window, ignoring other buttons
      Wait_For_Song : loop
         exit Wait_For_Song when List.Length > 0;

         Event := Ada_GUI.Next_Event;

         if not Event.Timed_Out then
            exit All_Cycles when Event.Event.Kind = Ada_GUI.Window_Closed;

            if Event.Event.Kind = Ada_GUI.Left_Click then
               exit All_Cycles when Event.Event.ID = Quit;

               if Event.Event.ID = Add then
                  Add_Song;
               elsif Event.Event.ID = Browse then
                  Browse_Songs;
               else
                  null;
               end if;
            end if;
         end if;
      end loop Wait_For_Song;

      All_Songs : loop
         Current := Song.Element (Index);

         if Sel_Available then
            Sel.Set_Selected (Index => Current.Position);
         end if;

         if Start (To_String (Current.Path) ) then
            Wait_For_End : loop
               Event := Ada_GUI.Next_Event (Timeout => 0.1);

               if Event.Timed_Out then
                  exit Wait_For_End when Player.Playback_Ended;
               elsif Event.Event.Kind = Ada_GUI.Window_Closed then
                  exit All_Cycles;
               elsif Event.Event.Kind = Ada_GUI.Left_Click then
                  exit All_Cycles when Event.Event.ID = Quit;

                  if Event.Event.ID = Play then
                     exit Wait_For_End when not Start (Sel.Text);
                  elsif Event.Event.ID = Skip then
                     Ada_GUI.Set_Title (Title => Title);

                     exit Wait_For_End;
                  elsif Event.Event.ID = Add then
                     Add_Song;
                  elsif Event.Event.ID = Browse then
                     Browse_Songs;
                  elsif Event.Event.ID = Delete then
                     Delete_Song;

                     if List.Length = 0 then
                        Player.Pause;

                        exit All_Songs;
                     end if;
                  else
                     null;
                  end if;
               else
                  null;
               end if;
            end loop Wait_For_End;
         end if;

         exit All_Cycles when Quit_After.Active;

         Index := Index + 1;

         if Index > Song.Last_Index then
            Index := 1;
         end if;
      end loop All_Songs;
   end loop All_Cycles;

   Quit_Now;
exception -- MP
when others =>
   if Ada_GUI.Set_Up then
      Quit_Now;
   end if;
end MP;
