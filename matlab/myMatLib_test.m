close all;clear all;clc
path2dll = 'myMatLib.dll';
path2hdr = 'myMatLib.h';

libName = 'myMatLib';

% Re-load library
try unloadlibrary(libName);end
loadlibrary(path2dll,path2hdr);

% Library function names
libfunctions('myMatLib')

% Matlab equivalent operation

% Call a fortran FUNCTION
% Fortran functions return a single value and can be defined easily in the
% C header, no jumping through loops here
calllib(libName,'test',pi)

a = rand(2,2)
b = libpointer('doublePtr',zeros(2,2));
calllib(libName,'test2',a,b)
sqrt(b.Value)

a = rand(4,4)
n = libpointer('doublePtr',size(a,1));
m = libpointer('doublePtr',size(a,2));
b = libpointer('doublePtr',zeros(size(a)));
tic
calllib(libName,'test3',n,m,a,b)
toc
sqrt(b.Value)

tic
for i = 1:100000000
  tmp = a.*a;
end
toc

% Call a fortran SUBROUTINE
% Subroutines do not return values so we must create a pointer to the value
% Create a libpointer object that we will use to extract the intent(out)
% values from the subroutine. 'doublePtr' with initial value of 1.
% The type of pointer will depend on the type of values being returned by
% the subroutine!
% yPtr = libpointer('doublePtr',1); 
% calllib(libName,'x_exp2',2,2,yPtr);
% ys = yPtr.Value 


unloadlibrary(libName);
%% Now call the matrix version
x = [1 2 3;4 5 6];
yPtr2 = libpointer('doublePtrPtr',x);
calllib(libName,'x_exp3',x,3,yPtr2)
ys = yPtr2.Value 

%%
unloadlibrary(libName);
