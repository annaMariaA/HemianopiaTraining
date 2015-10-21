%  example on how to flip images


%im = imread('../ClarkeCocoKeller2013/images/m_market_fruit.jpg');
mainFolder = 'ImagesObjectNaming/';
folder = 'ImagesObjectNaming/';% folder containing object stimuli
filelist = dir([folder '*.jpg']);
display.numImages = length(filelist);
for f = 1:length(filelist)
    [pict] = imread([folder filelist(f).name]);%repeat this one every image
    display.lineSegm(f).im = pict;
    
    display.lineSegm(f).number=filelist(f).name;
    display.lineSegm(f).name=[folder filelist(f).name];
    display.lineSegm(f).im = pict;

    flipped_im =display.lineSegm(f).im(:, end:-1:1,:);
    filename = [ 'f_', num2str(display.lineSegm(f).number), '.jpg'];
    imwrite(flipped_im,filename);
    imshow(flipped_im);
end