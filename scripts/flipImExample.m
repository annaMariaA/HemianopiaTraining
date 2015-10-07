%  example on how to flip images


im = imread('../ClarkeCocoKeller2013/images/m_market_fruit.jpg');
subplot(1,2,1);
imshow(im)

flipped_im = im(:, end:-1:1,:);
subplot(1,2,2);
imshow(flipped_im);
