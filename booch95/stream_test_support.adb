--  Copyright (C) 2002 Simon Wright.
--  All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

--  $Id$

package body Stream_Test_Support is


   procedure Write_Base_Class_P
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Base_Class_P) is
   begin
      Base'Class'Output (Stream, Obj.all);
   end Write_Base_Class_P;


   procedure Read_Base_Class_P
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Base_Class_P) is
      Result : constant Base'Class := Base'Class'Input (Stream);
   begin
      Obj := new Base'Class'(Result);
   end Read_Base_Class_P;


end Stream_Test_Support;
