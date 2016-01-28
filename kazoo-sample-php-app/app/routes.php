<?php

/*
|--------------------------------------------------------------------------
| Application Routes
|--------------------------------------------------------------------------
|
| Here is where you can register all of the routes for an application.
| It's a breeze. Simply tell Laravel the URIs it should respond to
| and give it the Closure to execute when that URI is requested.
|
*/

Route::get('/', 'HomeController@index');
Route::get('/accounts', 'AccountController@index');
Route::get('/accounts/{accountid}', 'AccountController@view');
Route::get('/accounts/new', 'AccountController@new');
Route::post('/accounts/create', 'AccountController@create');
Route::get('/accounts/{accountid}/edit', 'AccountController@edit');
Route::post('/accounts/{accountid}/update', 'AccountController@update');
Route::get('/accounts/{accountid}/delete', 'AccountController@delete');
Route::get('/devices', 'DeviceController@index');
Route::get('/devices/new', 'DeviceController@new');
Route::post('/devices/create', 'DeviceController@create');
Route::get('/devices/{deviceid}/edit', 'DeviceController@edit');
Route::post('/devices/{deviceid}/update', 'DeviceController@update');
Route::get('/devices/{deviceid}/delete', 'DeviceController@delete');