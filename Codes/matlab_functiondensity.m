% Define a range of values for x and z
x = linspace(0, 100, 100);  % Range of x values from 0 to 100
z = linspace(0, 100, 100);  % Range of z values from 0 to 100

% Create a grid of x and z values
[X, Z] = meshgrid(x, z);

% Calculate y values based on the function y = sqrt(x) * log(z)
Y = sqrt(X) .* log(Z);

% Plot the 3D surface
figure;
surf(X, Z, Y);
xlabel('x');
ylabel('z');
zlabel('y');
title('Spread of y = sqrt(x) * log(z)');