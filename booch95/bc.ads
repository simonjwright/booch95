package BC is

   -- This is the Top level package in the Booch Component Hierarchy.
   -- It serves as the repository of all exceptions possible in the components

   Reference_Violation : exception; 
   -- currently unused

   Abstraction_Violation : exception;
   -- Raised whenever a user attempts to use a Root type

end BC;

