function [p] = runModel();

clear;close;

function [h] = hypothesis(X,theta);
	h = X*theta';
end;

function [J,theta] = checkGradient(X,y,theta,lambda,alpha,num_iter);
	for i = 1:num_iter;
	h = hypothesis(X,theta);

	grad = alpha .* (mean((h.-y).*X) + (lambda.*mean([0 theta(2:end)]))); %regularised gradient
	theta = theta .- grad; 
 	
 	J(i) = computeCost(X,y,theta,lambda);
	end;
end;

function [J] = computeCost(X,y,theta,lambda);
h = hypothesis(X,theta);
J = (1/2) .* mean((h.-y).^2); %unregularised cost
J = J .+ (lambda.*(mean(theta(2:end).^2))); %regularised cost
end;


load('trainData_norm.txt');
load('testData_norm.txt');

X = [ones(length(trainData_norm),1) trainData_norm(:, 1:(size(trainData_norm, 2)-1))];
y = trainData_norm(:, size(trainData_norm,2));

X_test = [ones(length(testData_norm),1) testData_norm];

theta = zeros(1,size(X,2));
m = length(X);

[J, theta] = checkGradient(X,y,theta,0,0.1,100000);
min(J)

p = hypothesis(X_test,theta);

end;







