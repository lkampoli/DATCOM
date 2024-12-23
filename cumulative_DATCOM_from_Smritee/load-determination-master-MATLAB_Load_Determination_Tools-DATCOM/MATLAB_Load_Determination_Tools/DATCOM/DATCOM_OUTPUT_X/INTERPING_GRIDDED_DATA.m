% Define the sample points and values
X = [1 2 3 4 5];
V = [12 16 31 10 6];

% Specify the query points, Xq, that extend beyond the domain of X.
Xq = (0:0.1:6);
Vq = interp1(X,V,Xq,'pchip','extrap');

% Plot the results.
figure
plot(X,V,'o');
hold on
plot(Xq,Vq,'-');
legend('samples','pchip');
hold off

%%

% Define some data.

A = gallery('uniformdata',[3 5],0)

% Find 7th element in A
Seventh = A(7)