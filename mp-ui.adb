-- MP: a Music Player
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses
--
-- 2020-09-15     Initial version
--
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;
with Gnoga_File_Selection;

with PragmARC.B_Strings;
with PragmARC.Persistent_Skip_List_Unbounded;

package body MP.UI is
   use PragmARC.B_Strings;

   subtype Path_Name is B_String (Max_Length => 400);

   function Less (Left : Path_Name; Right : Path_Name) return Boolean;
   -- Files in the working directory are < files in subdirectories

   package Path_Lists is new PragmARC.Persistent_Skip_List_Unbounded (Element => Path_Name, "<" => Less);

   Window     : Gnoga.Gui.Window.Window_Type;
   View       : Gnoga.Gui.View.View_Type;
   Player     : Gnoga.Gui.Element.Multimedia.Audio_Type;
   Form       : Gnoga.Gui.Element.Form.Form_Type;
   Sel        : Gnoga.Gui.Element.Form.Selection_Type;
   Count      : Gnoga.Gui.Element.Form.Number_Type;
   Cnt_Lbl    : Gnoga.Gui.Element.Form.Label_Type;
   Delete     : Gnoga.Gui.Element.Common.Button_Type;
   Path       : Gnoga.Gui.Element.Form.Text_Type;
   Browse     : Gnoga.Gui.Element.Common.Button_Type;
   Add        : Gnoga.Gui.Element.Common.Button_Type;
   Play       : Gnoga.Gui.Element.Common.Button_Type;
   Skip       : Gnoga.Gui.Element.Common.Button_Type;
   Quit       : Gnoga.Gui.Element.Common.Button_Type;
   Quit_After : Gnoga.Gui.Element.Form.Check_Box_Type;
   QA_LbL     : Gnoga.Gui.Element.Form.Label_Type;

   function Less (Left : Path_Name; Right : Path_Name) return Boolean is
      function Slash_Count (Path : Path_Name) return Natural;

      function Slash_Count (Path : Path_Name) return Natural is
         Result : Natural := 0;

         Path_S : constant String := +Path;
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

   task DJ is
      entry Start;
      entry Play;
      entry Skip;
      entry Quit;
   end DJ;

   procedure Play_Selected (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- Empty;
   begin -- Play_Selected
      DJ.Play;
   exception -- Play_Selected
   when E : others =>
      Gnoga.Log (Message => "Play_Selected: " & Ada.Exceptions.Exception_Information (E) );
   end Play_Selected;

   procedure Skip_To_Next (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- Empty;
   begin -- Skip_To_Next
      DJ.Skip;
   exception -- Skip_To_Next
   when E : others =>
      Gnoga.Log (Message => "Skip_To_Next: " & Ada.Exceptions.Exception_Information (E) );
   end Skip_To_Next;

   procedure Quit_Now (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      -- Empty;
   begin -- Quit_Now
      DJ.Quit;

      delay 0.1;

      Gnoga.Application.Singleton.End_Application;
   exception -- Quit_Now
   when E : others =>
      Gnoga.Log (Message => "Quit_Now: " & Ada.Exceptions.Exception_Information (E) );
   end Quit_Now;

   type Song_Info is record
      Position : Positive;
      Path     : Path_Name;
   end record;

   package Song_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Song_Info);

   List : Path_Lists.Persistent_Skip_List := Path_Lists.Open_List ("playlist.mpl");
   Song : Song_Lists.Vector;

   procedure Make_Song_List (List : in out Song_Lists.Vector) is
      Position : Natural := 0;

      procedure Add_One (Item : in Path_Name; Continue : out Boolean) is
         -- Empty;
      begin -- Add_One
         Continue := True;
         Position := Position + 1;
         List.Append (New_Item => (Position => Position, Path => Item) );
         Sel.Add_Option (Value => +Item, Text => +Item);
      end Add_One;

      procedure Add_All is new Path_Lists.Iterate (Action => Add_One);
   begin -- Make_Song_List
      List.Clear;
      Add_All (List => UI.List);
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

   procedure Refresh is
      -- Empty;
   begin -- Refresh
      Clear_Sel : for I in reverse 1 .. Sel.Length loop
         Sel.Remove_Option (I);
      end loop Clear_Sel;

      Make_Song_List (List => Song);
      Count.Value (Value => Sel.Length);
      Shuffle (List => Song);
   end Refresh;

   procedure Delete_Song (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Index : constant Natural := Sel.Selected_Index;

      Path   : Path_Name;
      Paused : Boolean := False;
   begin -- Delete_Song
      if Index = 0 then
         return;
      end if;

      if not Player.Playback_Ended then
         Paused := True;
         Player.Pause;
      end if;

      Path.Assign (From => Sel.Text (Index) );
      List.Delete (Item => Path);
      Refresh;

      if Paused then
         Player.Play;
      end if;
   exception -- Delete_Song
   when E : others =>
      Gnoga.Log (Message => "Delete_Song: " & Ada.Exceptions.Exception_Information (E) );
   end Delete_Song;

   Current_Directory : constant String := Ada.Directories.Current_Directory;

   procedure Browse_Songs (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Result : constant Gnoga_File_Selection.Result_Info :=
         Gnoga_File_Selection.Select_File (Window, Current_Directory);
   begin -- Browse_Songs
      if Result.Picked then
         Get_Name : declare
            Name : constant String := Ada.Strings.Unbounded.To_String (Result.Value);
         begin -- Get_Name
            if Name'Length > Current_Directory'Length and then
               Name (Name'First .. Name'First + Current_Directory'Length - 1) = Current_Directory
            then
               Path.Value (Value => Name (Name'First + Current_Directory'Length + 1 .. Name'Last) );
            end if;
         end Get_Name;
      end if;
   end Browse_Songs;

   procedure Add_Song (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Name : constant String := Path.Value;

      Path   : Path_Name;
      Paused : Boolean := False;
   begin -- Add_Song
      if Name'Length = 0 then
         return;
      end if;

      if not Player.Playback_Ended then
         Paused := True;
         Player.Pause;
      end if;

      Path.Assign (From => Name);
      List.Insert (Item => Path);
      Refresh;
      UI.Path.Value (Value => "");

      if Paused then
         Player.Play;
      end if;
   exception -- Add_Song
   when E : others =>
      Gnoga.Log (Message => "Add_Song: " & Ada.Exceptions.Exception_Information (E) );
   end Add_Song;

   task body DJ is
      function Start (Song : in String) return Boolean is -- Returns True if Song started; False otherwise
        -- Empty;
      begin -- Start
         Player.Media_Source (Source => Song);
         Window.Document.Title (Value => "MP " & Song);

         Wait_For_Ready : for I in 1 .. 10 loop
            if Player.Ready_To_Play then
               Player.Play;

               return True;
            end if;

            delay 0.01;
         end loop Wait_For_Ready;

         return False;
      end Start;

      Current : Song_Info;
      Index   : Positive := 1;
      Run     : Boolean := True;
   begin -- DJ
      Wait_For_Song : loop
         exit Wait_For_Song when Song.Last_Index > 0;

         select
            accept Quit;
            Window.Document.Title (Value => "MP");
            Run := False;
         or
            delay 1.0;
         end select;
      end loop Wait_For_Song;

      if Run then
         accept Start;

         Forever : loop
            Current := Song.Element (Index);
            Sel.Selected (Index => Current.Position);

            if Start (+Current.Path) then
               Wait_For_End : loop
                  select
                     accept Play;

                     exit Wait_For_End when not Start (Sel.Text (Sel.Selected_Index) );
                  or
                     accept Skip;

                     Window.Document.Title (Value => "MP");

                     exit Wait_For_End;
                  or
                     accept Quit;

                     Window.Document.Title (Value => "MP");

                     exit Forever;
                  or
                     delay 1.0;

                     exit Wait_For_End when Player.Playback_Ended;
                  end select;
               end loop Wait_For_End;
            end if;

            exit Forever when Quit_After.Checked;

            Index := Index + 1;

            if Index > Song.Last_Index then
               Index := 1;
            end if;
         end loop Forever;

         if Quit_After.Checked then
            Window.Document.Title (Value => "MP");
            Gnoga.Application.Singleton.End_Application;
         end if;
      end if;
   exception -- DJ
   when E : others =>
      Gnoga.Log (Message => "DJ: " & Ada.Exceptions.Exception_Information (E) );
   end DJ;
begin -- MP.UI
   Gnoga.Application.Title (Name => "MP");
   Gnoga.Application.HTML_On_Close (HTML => "MP ended.");
   Gnoga.Application.Open_URL (url => "http://localhost:8089/");
   Gnoga.Application.Singleton.Initialize (Main_Window => Window, Port => 8089);
   View.Create (Parent => Window);
   View.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Player.Create (Parent => View, Preload => True);
   View.New_Line;
   Form.Create (Parent => View);
   Form.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Sel.Create (Form => Form, Visible_Lines => 30);
   Form.New_Line;

   Count.Create (Form => Form);
   Count.Editable (Value => False);
   Count.Read_Only;
   Cnt_Lbl.Create (Form => Form, Label_For => Count, Content => "Number of songs:");
   Form.New_Line;
   Delete.Create (Parent => Form, Content => "Delete");
   Delete.On_Click_Handler (Handler => Delete_Song'Access);
   Path.Create (Form => Form, Size => 100);
   Browse.Create (Parent => Form, Content => "Browse");
   Browse.On_Click_Handler (Handler => Browse_Songs'Access);
   Add.Create (Parent => Form, Content => "Add");
   Add.On_Click_Handler (Handler => Add_Song'Access);
   Form.New_Line;
   Play.Create (Parent => Form, Content => "Play");
   Play.On_Click_Handler (Handler => Play_Selected'Access);
   Skip.Create (Parent => Form, Content => "Skip");
   Skip.On_Click_Handler (Handler => Skip_To_Next'Access);
   Quit.Create (Parent => Form, Content => "Quit");
   Quit.On_Click_Handler (Handler => Quit_Now'Access);
   Quit_After.Create (Form => Form);
   QA_LbL.Create (Form => Form, Label_For => Quit_After, Content => "Quit after this song", Auto_Place => False);

   Refresh;
   DJ.Start;

   Gnoga.Application.Singleton.Message_Loop;
exception -- MP.UI
when E : others =>
   Gnoga.Log (Message => Ada.Exceptions.Exception_Information (E) );
end MP.UI;
