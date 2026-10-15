function [ mini, maxi ] = minmax( stack )
mini = min( stack, [], 'all' );
maxi = max( stack, [], 'all' );
end